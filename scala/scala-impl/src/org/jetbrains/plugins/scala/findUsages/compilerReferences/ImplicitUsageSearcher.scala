package org.jetbrains.plugins.scala.findUsages.compilerReferences

import java.awt._
import java.text.MessageFormat

import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.ui.DialogWrapper
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.search.{RequestResultProcessor, TextOccurenceProcessor, UsageSearchContext}
import com.intellij.psi.util.PsiUtilCore
import com.intellij.psi.{PsiElement, PsiReference}
import com.intellij.ui.{CheckBoxList, JBColor, ScrollPaneFactory}
import com.intellij.util.ui.JBUI
import com.intellij.util.{Processor, QueryExecutor}
import javax.swing._
import javax.swing.border.MatteBorder
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.findUsages.compilerReferences.ImplicitUsageSearcher.ImplicitFindUsagesDialog
import org.jetbrains.plugins.scala.finder.ScalaFilterScope
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScModifierListOwner, ScNamedElement}

import scala.collection.JavaConverters._

abstract class ImplicitUsageSearcher extends QueryExecutor[PsiReference, ReferencesSearch.SearchParameters] {
  override def execute(
    parameters: ReferencesSearch.SearchParameters,
    consumer: Processor[PsiReference]
  ): Boolean = parameters.getElementToSearch match {
    case ImplicitMember(member) =>
      val dirtyModules = dirtyModuleInDependencyChain(member)

      val shouldStop = if (dirtyModules.nonEmpty) {
        var exitCode     = DialogWrapper.OK_EXIT_CODE
        val dialogAction = () => showRebuildSuggestionDialog(dirtyModules, member)(r => exitCode = r)

        if (SwingUtilities.isEventDispatchThread) dialogAction()
        else invokeAndWait(dialogAction())
        exitCode == DialogWrapper.CANCEL_EXIT_CODE
      } else false

      if (!shouldStop) {
        val optimizer = parameters.getOptimizer

        optimizer.searchWord(
          member.name,
          ScalaFilterScope(parameters),
          UsageSearchContext.IN_CODE,
          true,
          member,
          requestResultProcessor(member)
        )
      }
      false
    case _ => true
  }

  protected def requestResultProcessor(target: ScMember): TextOccurenceProcessor

  private object ImplicitMember {
    def unapply(e: PsiElement): Option[ScMember] = e match {
      case ScalaPsiUtil.inNameContext(member: ScMember) if ScalaPsiUtil.isImplicit(member: ScModifierListOwner) =>
        Some(member)
      case _ => None
    }
  }

  private def showRebuildSuggestionDialog[T](
    dirtyModules: Seq[Module],
    element: ScNamedElement
  )(callback: Int => T): Unit = {
    val dialog = new ImplicitFindUsagesDialog(false, dirtyModules, element)
    dialog.show()
    val exitCode = dialog.getExitCode
    callback(exitCode)
  }

  private def dirtyModuleInDependencyChain(element: PsiElement): Seq[Module] = inReadAction {
    val project      = element.getProject
    val file         = PsiUtilCore.getVirtualFile(element)
    val index        = ProjectFileIndex.getInstance(project)
    val modules      = index.getOrderEntriesForFile(file).asScala.map(_.getOwnerModule)
    val service      = ScalaCompilerReferenceService.getInstance(project)
    val dirtyModules = service.getDirtyScopeHolder.getAllDirtyModules
    modules.filter(dirtyModules.contains)
  }
}

object ImplicitUsageSearcher {
  private class ImplicitFindUsagesDialog(
    canBeParent: Boolean,
    dirtyModules: Seq[Module],
    element: ScNamedElement
  ) extends DialogWrapper(inReadAction(element.getProject), canBeParent) {
    setTitle(ScalaBundle.message("find.usages.implicit.dialog.title"))
    init()

    override def createActions(): Array[Action] = super.createActions()

    private[this] val description: String =
      """
        |<html>
        |<body>
        |Implicit usages search is only supported inside a compiled scope,
        |but the use scope of member <code>{0}</code> contains dirty modules.
        |You can:<br>
        |-&nbsp;Rebuild some modules before proceeding, or<br>
        |-&nbsp;Only search for usages in already up-to-date modules.<br>
        |<br>
        |Check modules you want to rebuild:
        |</body>
        |</html>
        |"""".stripMargin

    private def createDescriptionLabel: JComponent = {
      val message = MessageFormat.format(description, element.name)
      new JLabel(message)
    }

    private def createModulesList: JComponent = {
      val dirtyModulesList = new CheckBoxList[Module]()

      applyTo(dirtyModulesList)(
        _.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION),
        _.setItems(dirtyModules.asJava, _.getName),
        _.setBorder(JBUI.Borders.empty(5))
      )

      val modulesScrollPane = ScrollPaneFactory.createScrollPane(
        dirtyModulesList,
        ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED
      )

      modulesScrollPane.setBorder(new MatteBorder(0, 0, 1, 0, JBColor.border()))
      modulesScrollPane.setMaximumSize(new Dimension(-1, 300))
      modulesScrollPane
    }

    override def createCenterPanel(): JComponent =
      applyTo(new JPanel(new BorderLayout()))(
        _.add(createModulesList)
      )

    override def createNorthPanel(): JComponent = {
      val gbConstraints = new GridBagConstraints
      val panel         = new JPanel(new GridBagLayout)
      gbConstraints.insets = JBUI.insets(4, 0, 10, 8)
      panel.add(createDescriptionLabel, gbConstraints)
      panel
    }
  }
}
