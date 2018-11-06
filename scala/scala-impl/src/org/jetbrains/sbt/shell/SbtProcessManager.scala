package org.jetbrains.sbt.shell

import java.io.{File, IOException, OutputStreamWriter, PrintWriter}

import com.intellij.debugger.engine.DebuggerUtils
import com.intellij.execution.configurations._
import com.intellij.execution.process.ColoredProcessHandler
import com.intellij.notification.{Notification, NotificationAction, NotificationType}
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.options.ShowSettingsUtil
import com.intellij.openapi.options.ex.SingleConfigurableEditor
import com.intellij.openapi.options.newEditor.SettingsDialog
import com.intellij.openapi.project.{Project, ProjectUtil}
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.roots.ui.configuration.ProjectStructureConfigurable
import com.intellij.openapi.ui.DialogWrapper.DialogStyle
import com.intellij.openapi.ui.MessageType
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.VfsUtil
import org.jetbrains.plugins.scala.buildinfo.BuildInfo
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.{CompilerIndicesSbtSettings, CompilerIndicesSettings}
import org.jetbrains.plugins.scala.project.Version
import org.jetbrains.plugins.scala.project.external.{JdkByName, SdkUtils}
import org.jetbrains.sbt.{Sbt, SbtUtil}
import org.jetbrains.sbt.SbtUtil._
import org.jetbrains.sbt.project.settings.{SbtExecutionSettings, SbtProjectSettings}
import org.jetbrains.sbt.project.structure.{JvmOpts, SbtOpts}
import org.jetbrains.sbt.project.{SbtExternalSystemManager, SbtProjectResolver, SbtProjectSystem}

import scala.collection.JavaConverters._
/**
  * Manages the sbt shell process instance for the project.
  * Instantiates an sbt instance when initially requested.
  *
  * Created by jast on 2016-11-27.
  */
class SbtProcessManager(project: Project) extends ProjectComponent {

  import SbtProcessManager.ProcessData

  private val log = Logger.getInstance(getClass)

  @volatile private var processData: Option[ProcessData] = None

  private val repoPath = normalizePath(getRepoDir)

  private def pluginResolverSetting: String =
    raw"""resolvers += Resolver.file("intellij-scala-plugin", file(raw"$repoPath"))(Resolver.ivyStylePatterns)"""


  /** Plugins injected into user's global sbt build. */
  // TODO add configurable plugins somewhere for users and via API; factor this stuff out
  private def injectedPlugins(sbtMajorVersion: Version): Seq[String] =
    sbtStructurePlugin(sbtMajorVersion)

  // this *might* get messy if multiple IDEA projects start messing with the global settings.
  // but we should be fine since it is written before every sbt boot
  private def sbtStructurePlugin(sbtMajorVersion: Version): Seq[String] = {
    val sbtStructureVersion           = BuildInfo.sbtStructureVersion
    val sbtIdeaShellVersion           = BuildInfo.sbtIdeaShellVersion

    val compilerIndicesEnabled = CompilerIndicesSettings(project).indexingEnabled
    val compilerIndicesPlugin  = compilerIndicesEnabled.seq {
      val pluginVersion = BuildInfo.sbtIdeaCompilerIndicesVersion
      s"""addSbtPlugin("org.jetbrains" % "sbt-idea-compiler-indices" % "$pluginVersion")"""
    }


    sbtMajorVersion.presentation match {
      case "0.12" => Seq.empty // 0.12 doesn't support AutoPlugins
      case _ => Seq(
        s"""addSbtPlugin("org.jetbrains" % "sbt-structure-extractor" % "$sbtStructureVersion")""",
        s"""addSbtPlugin("org.jetbrains" % "sbt-idea-shell" % "$sbtIdeaShellVersion")""",
      ) ++ compilerIndicesPlugin // works for 0.13.5+, for older versions it will be ignored
    }
  }


  private def createShellProcessHandler(): (ColoredProcessHandler, Option[RemoteConnection]) = {
    val workingDirPath =
      Option(ProjectUtil.guessProjectDir(project))
        .getOrElse(throw new IllegalStateException(s"no project directory found for project ${project.getName}"))
        .getCanonicalPath
    val workingDir = new File(workingDirPath)

    val sbtSettings = getSbtSettings(workingDirPath)
    lazy val launcher = launcherJar(sbtSettings)

    // an id to identify this boot of sbt as being launched from idea, so that any plugins it injects are never ever loaded otherwise
    // use sbtStructureVersion as approximation of compatible versions of IDEA this is allowed to launch with.
    // this avoids failing reloads when multiple sbt instances are booted from IDEA (SCL-12009)
    val runid = BuildInfo.sbtStructureVersion

    val sdk = selectSdkOrWarn(sbtSettings)
    val javaParameters: JavaParameters = new JavaParameters
    javaParameters.setJdk(sdk)
    javaParameters.configureByProject(project, 1, sdk)
    javaParameters.setWorkingDirectory(workingDir)
    javaParameters.setJarPath(launcher.getCanonicalPath)

    val debugConnection = if (sbtSettings.shellDebugMode) Option(addDebugParameters(javaParameters)) else None

    val projectSbtVersion = detectSbtVersion(workingDir, launcher)

    // to use the plugin injection command, we may have to override older sbt versions where possible
    val shouldUpgradeSbtVersion =
      sbtSettings.allowSbtVersionOverride &&
        projectSbtVersion >= Version("1.0.0") &&
        projectSbtVersion < Sbt.LatestVersion

    val upgradedSbtVersion =
      if (shouldUpgradeSbtVersion) SbtUtil.latestCompatibleVersion(projectSbtVersion)
      else projectSbtVersion

    val autoPluginsSupported = upgradedSbtVersion >= SbtProjectResolver.sinceSbtVersionShell
    val addPluginCommandSupported = upgradedSbtVersion >= SbtProcessManager.addPluginCommandVersion

    val vmParams = javaParameters.getVMParametersList
    vmParams.add("-server")
    vmParams.addAll(SbtOpts.loadFrom(workingDir).asJava)
    vmParams.addAll(JvmOpts.loadFrom(workingDir).asJava)
    vmParams.addAll(sbtSettings.vmOptions.asJava)
    // don't add runid when using addPluginFile command
    if (! addPluginCommandSupported)
      vmParams.add(s"-Didea.runid=$runid")
    if (shouldUpgradeSbtVersion)
      vmParams.add(s"-Dsbt.version=$upgradedSbtVersion")

    val commandLine: GeneralCommandLine = javaParameters.toCommandLine
    getCustomVMExecutableOrWarn(sbtSettings).foreach(exe => commandLine.setExePath(exe.getAbsolutePath))

    if (autoPluginsSupported) {
      val sbtMajorVersion = binaryVersion(upgradedSbtVersion)

      val globalPluginsDir = globalPluginsDirectory(sbtMajorVersion, commandLine.getParametersList)
      // workaround: --addPluginsSbtFile fails if global plugins dir does not exist. https://youtrack.jetbrains.com/issue/SCL-14415
      globalPluginsDir.mkdirs()

      val globalSettingsFile = new File(globalPluginsDir, "idea.sbt")

      val settingsFile =
        if (addPluginCommandSupported) FileUtil.createTempFile("idea",".sbt", true)
        else globalSettingsFile

      // caution! writes injected plugin settings to user's global sbt config if addPlugin command is not supported
      val plugins = injectedPlugins(sbtMajorVersion)
      injectSettings(runid, ! addPluginCommandSupported, settingsFile, pluginResolverSetting +: plugins)

      if (addPluginCommandSupported)
        commandLine.addParameter(s"--addPluginSbtFile=${settingsFile.getAbsolutePath}")

      // we have our plugins in there, load custom shell
      val compilerIndicesPluginLoaded = plugins.exists(_.contains("sbt-idea-compiler-indices"))
      val ideaPort = CompilerIndicesSbtSettings().sbtConnectionPort
      val commands = compilerIndicesPluginLoaded.fold(
        s""""; set ideaPort in Global := $ideaPort ; idea-shell"""",
        "idea-shell"
      )
      commandLine.addParameter(commands)
    }

    if (shouldUpgradeSbtVersion)
      notifyVersionUpgrade(projectSbtVersion, upgradedSbtVersion, workingDir)

    val pty = createPtyCommandLine(commandLine)
    val cpty = new ColoredProcessHandler(pty)

    (cpty, debugConnection)
  }

  private def notifyVersionUpgrade(projectSbtVersion: Version, upgradedSbtVersion: Version, projectPath: File): Unit = {
    val message =
      s"Started sbt shell with sbt version ${upgradedSbtVersion.presentation} instead of ${projectSbtVersion.presentation} configured by project."
    val notification = SbtShellNotifications.notificationGroup.createNotification(message, MessageType.INFO)

    notification.addAction(new UpdateSbtVersionAction(projectPath))
    notification.addAction(DisableSbtVersionOverrideAction)
    notification.notify(project)
  }

  private object DisableSbtVersionOverrideAction extends NotificationAction("Disable version override") {
    override def actionPerformed(anActionEvent: AnActionEvent, notification: Notification): Unit = {
      SbtProjectSettings.forProject(project).setAllowSbtVersionOverride(false)
      notification.expire()
    }
  }

  private class UpdateSbtVersionAction(projectPath: File)
    extends NotificationAction(s"Update sbt version") {
    override def actionPerformed(anActionEvent: AnActionEvent, notification: Notification): Unit = {
      val propertiesFile = SbtUtil.sbtBuildPropertiesFile(projectPath)
      val vFile = VfsUtil.findFileByIoFile(propertiesFile, true)
      new OpenFileDescriptor(project, vFile).navigate(true)
      notification.expire()
    }
  }

  /** If a custom VM executable is configured, return it. If it's not a valid path, warn user. */
  private def getCustomVMExecutableOrWarn(sbtSettings: SbtExecutionSettings) =
    Option(sbtSettings.vmExecutable).filter { path =>
      if (path.isFile) true
      else {
        val badCustomVMNotification =
          SbtShellNotifications.notificationGroup
            .createNotification(s"No JRE found at path ${sbtSettings.vmExecutable}. Using project JDK instead.", NotificationType.WARNING)
        badCustomVMNotification.addAction(ConfigureSbtAction)
        badCustomVMNotification.notify(project)
        false
      }
    }

  private object ConfigureSbtAction extends NotificationAction("&Configure sbt VM") {

    override def actionPerformed(e: AnActionEvent, notification: Notification): Unit = {
      // External system handles the Configurable name for sbt settings
      ShowSettingsUtil.getInstance().showSettingsDialog(project, SbtProjectSystem.Id.getReadableName)
      notification.expire()
    }
  }

  private def selectSdkOrWarn(sbtSettings: SbtExecutionSettings): Sdk = {

    val configuredSdk = sbtSettings.jdk.map(JdkByName).flatMap(SdkUtils.findProjectSdk)
    val projectSdk = ProjectRootManager.getInstance(project).getProjectSdk

    configuredSdk.getOrElse {
      if (projectSdk != null) projectSdk
      else {
        val message = "No project JDK configured, but it is required to run sbt shell."
        val noProjectSdkNotification = SbtShellNotifications.notificationGroup
          .createNotification(message, NotificationType.ERROR)
        noProjectSdkNotification.addAction(ConfigureProjectJdkAction)
        noProjectSdkNotification.notify(project)
        throw new RuntimeException(message)
      }
    }
  }

  private object ConfigureProjectJdkAction extends NotificationAction("&Configure project jdk") {
    override def startInTransaction(): Boolean = true

    // copied from ShowStructureSettingsAction
    override def actionPerformed(e: AnActionEvent, notification: Notification): Unit = {
      new SingleConfigurableEditor(project, ProjectStructureConfigurable.getInstance(project), SettingsDialog.DIMENSION_KEY) {
        override protected def getStyle = DialogStyle.COMPACT
      }.show()
      notification.expire()
    }
  }

  /**
    * add debug parameters to java parameters and create remote connection
    * @return
    */
  private def addDebugParameters(javaParameters: JavaParameters): RemoteConnection = {

    val host = "localhost"
    val port = DebuggerUtils.getInstance.findAvailableDebugAddress(true)
    val remoteConnection = new RemoteConnection(true, host, port, false)

    val shellDebugProperties = s"-agentlib:jdwp=transport=dt_socket,address=$host:$port,suspend=n,server=y"
    val vmParams = javaParameters.getVMParametersList
    vmParams.prepend("-Xdebug")
    vmParams.replaceOrPrepend("-agentlib:jdwp=", shellDebugProperties)

    remoteConnection
  }

  private def getSbtSettings(dir: String) = SbtExternalSystemManager.executionSettingsFor(project, dir)

  private def launcherJar(sbtSettings: SbtExecutionSettings): File =
    sbtSettings.customLauncher.getOrElse(getDefaultLauncher)

  /**
    * Because the regular GeneralCommandLine process doesn't mesh well with JLine on Windows, use a
    * Pseudo-Terminal based command line
    * @param commandLine commandLine to copy from
    * @return
    */
  private def createPtyCommandLine(commandLine: GeneralCommandLine) = {
    val pty = new PtyCommandLine()
    pty.withExePath(commandLine.getExePath)
    pty.withWorkDirectory(commandLine.getWorkDirectory)
    pty.withEnvironment(commandLine.getEnvironment)
    pty.withParameters(commandLine.getParametersList.getList)
    pty.withParentEnvironmentType(commandLine.getParentEnvironmentType)

    pty
  }

  /** Request an sbt shell process instance. It will be started if necessary.
    * The process handler should only be used to access the running process!
    * SbtProcessManager is solely responsible for handling the running state.
    */
  private[shell] def acquireShellProcessHandler: ColoredProcessHandler = processData.synchronized {
    processData.exists(_.processHandler.getProcess.isAlive)
    processData match {
      case Some(ProcessData(handler, _)) if handler.getProcess.isAlive =>
        handler
      case _ =>
        updateProcessData().processHandler
    }
  }

  /**
    * Inject custom settings or plugins into an sbt directory.
    * This seems to be the most straightforward way to add our own sbt settings
    */
  private def injectSettings(runid: String, guardSettings: Boolean, settingsFile: File, settings: Seq[String]): Unit = {
    val header =
      """// Generated by IntelliJ-IDEA Scala plugin.
        |// Adds settings when starting sbt from IDEA.
        |// Manual changes to this file will be lost.
      """.stripMargin.trim
    val settingsString = settings.mkString("scala.collection.Seq(\n",",\n","\n)")

    // any idea-specific settings should be added conditional on sbt being started from idea
    val guardedSettings =
      if (guardSettings)
        s"""if (java.lang.System.getProperty("idea.runid", "false") == "$runid") $settingsString else scala.collection.Seq.empty"""
      else settingsString

    val content = header + "\n" + guardedSettings

    try {
      FileUtil.writeToFile(settingsFile, content)
    } catch {
      case x : IOException =>
        log.error(s"unable to write ${settingsFile.getPath} which is required for sbt shell support", x)
        throw x
    }
  }

  private def updateProcessData(): ProcessData = {
    val (handler, debugConnection) = createShellProcessHandler()

    val title = project.getName
    val runner = new SbtShellRunner(project, title, debugConnection)
    runner.createConsoleView() // force creation now so that it's not null later and to avoid UI hanging

    val pd = ProcessData(handler, runner)

    processData.synchronized {
      processData = Option(pd)
      runner.initAndRun()
    }

    pd
  }

  /** Supply a PrintWriter that writes to the current process. */
  def usingWriter[T](f: PrintWriter => T): T = {
    val writer = new PrintWriter(new OutputStreamWriter(acquireShellProcessHandler.getProcessInput))
    f(writer)
  }

  /** Creates the SbtShellRunner view if necessary. */
  def acquireShellRunner: SbtShellRunner = processData.synchronized {

    processData match {
      case Some(ProcessData(_, runner)) if runner.getConsoleView.isRunning =>
        runner
      case _ =>
        updateProcessData().runner
    }
  }

  def restartProcess(): Unit = processData.synchronized {
    destroyProcess()
    updateProcessData()
  }

  def destroyProcess(): Unit = processData.synchronized {
    processData match {
      case Some(ProcessData(handler, runner)) =>
        Disposer.dispose(runner)
        handler.destroyProcess()
        processData = None
      case None => // nothing to do
    }
  }

  override def projectClosed(): Unit = {
    destroyProcess()
  }

  /** Report if shell process is alive. Should only be used for UI/informational purposes. */
  def isAlive: Boolean =
    processData.exists(_.processHandler.getProcess.isAlive)
}

object SbtProcessManager {
  def forProject(project: Project): SbtProcessManager = {
    val pm = project.getComponent(classOf[SbtProcessManager])
    if (pm == null) throw new IllegalStateException(s"unable to get component SbtProcessManager for project $project")
    else pm
  }

  private case class ProcessData(processHandler: ColoredProcessHandler,
                                 runner: SbtShellRunner)

  /** Since version 1.2.0 sbt supports injecting additional plugins to the sbt shell with a command.
    * This allows injecting plugins without messing with the user's global directory.
    * https://github.com/sbt/sbt/pull/4211
    */
  val addPluginCommandVersion = Version("1.2.0")
}
