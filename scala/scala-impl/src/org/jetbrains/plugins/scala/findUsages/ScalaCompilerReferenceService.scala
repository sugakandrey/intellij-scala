package org.jetbrains.plugins.scala.findUsages

import java.util
import java.util.UUID
import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import com.intellij.compiler.server.BuildManagerListener
import com.intellij.compiler.{CompilerDirectHierarchyInfo, CompilerReferenceService}
import com.intellij.ide.highlighter.{JavaClassFileType, JavaFileType}
import com.intellij.openapi.compiler.{CompilationStatusListener, CompileContext, CompilerPaths, CompilerTopics}
import com.intellij.openapi.fileTypes.FileType
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs._
import com.intellij.openapi.vfs.newvfs.BulkFileListener
import com.intellij.openapi.vfs.newvfs.events.VFileEvent
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiElement, PsiNamedElement}
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.project._

class ScalaCompilerReferenceService(
  project: Project
) extends CompilerReferenceService(project) { self =>
  private[this] val fileTypes     = Set(ScalaFileType.INSTANCE, JavaFileType.INSTANCE, JavaClassFileType.INSTANCE)
  private[this] val lock          = new ReentrantReadWriteLock()
  private[this] val readerLock    = lock.readLock()
  private[this] val openCloseLock = lock.writeLock()

  override def projectOpened(): Unit = {
    val connection = project.getMessageBus.connect(project)
    println(s"project opened")
    connection.subscribe(BuildManagerListener.TOPIC, new BuildManagerListener {
      override def buildStarted(project: Project, sessionId: UUID, isAutomake: Boolean): Unit = ()
        if  (project == self.project) closeReaderIfNeed(IndexCloseReason.COMPILATION_STARTED)
    })

    connection.subscribe(CompilerTopics.COMPILATION_STATUS, new CompilationStatusListener {
      override def compilationFinished(
        aborted: Boolean,
        errors: Int,
        warnings: Int,
        compileContext: CompileContext
      ): Unit = ()
    })

    connection.subscribe(VirtualFileManager.VFS_CHANGES, new ClassFileModificationListener)
//    connection.subscribe(VirtualFileManager.VFS_CHANGES, new SourceFileDeletionListener)
  }

  override def projectClosed(): Unit = {}

  private def outputDirs(module: Module): Seq[VirtualFile] =
    Seq(
      CompilerPaths.getModuleOutputDirectory(module, false),
      CompilerPaths.getModuleOutputDirectory(module, true)
    ).filter(_ != null)

  private def projectOutputDirs: Set[VirtualFile] =
    project.scalaModules.flatMap(outputDirs(_))(collection.breakOut)

  private def isClassfileUnderOutputDirectory(file: VirtualFile): Boolean =
    Option(file.getExtension).forall(_ == "class") && projectOutputDirs.exists(VfsUtilCore.isAncestor(_, file, true))

  private def onFileChange(file: VirtualFile): Unit = if (isClassfileUnderOutputDirectory(file)) {
    println(s"file = ${if (file != null) file.toString}")
  }

  override def isActive: Boolean = ???

  override def getScopeWithoutCodeReferences(element: PsiElement): GlobalSearchScope = ???

  override def getDirectInheritors(
    aClass: PsiNamedElement,
    useScope: GlobalSearchScope,
    searchScope: GlobalSearchScope,
    searchFileType: FileType
  ): CompilerDirectHierarchyInfo = ???

  override def getCompileTimeOccurrenceCount(element: PsiElement, isConstructorCompletion: Boolean) = ???

  override def getFunExpressions(
    functionalInterface: PsiNamedElement,
    useScope: GlobalSearchScope,
    searchScope: GlobalSearchScope,
    searchFileType: FileType
  ): CompilerDirectHierarchyInfo = ???

  private class ClassFileModificationListener extends BulkFileListener {
    override def after(events: util.List[_ <: VFileEvent]): Unit = events.forEach(println(_))
//      events.forEach(e => onFileChange(e.getFile))
  }

  private class SourceFileDeletionListener extends BulkFileListener {
    override def after(events: util.List[_ <: VFileEvent]): Unit = super.after(events)
  }
}
