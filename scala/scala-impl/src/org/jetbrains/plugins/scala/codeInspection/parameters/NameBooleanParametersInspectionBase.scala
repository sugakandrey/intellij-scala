package org.jetbrains.plugins.scala
package codeInspection
package parameters

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScBooleanLiteral, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.ScTypeExt
import org.jetbrains.plugins.scala.lang.psi.types.api.Boolean
import org.jetbrains.plugins.scala.lang.typeInference.Parameter
import org.jetbrains.plugins.scala.project.ProjectContext

/**
  * @author Ksenia.Sautina
  * @since 5/10/12
  */
abstract class NameBooleanParametersInspectionBase extends LocalInspectionTool {

  import NameBooleanParametersInspectionBase._

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = {
    new ScalaElementVisitor {
      override def visitMethodCallExpression(mc: ScMethodCall) {
        if (mc == null || mc.args == null || mc.args.exprs.isEmpty) return
        if (isIgnoreSingleParameter && isSingleParamMethodCall(mc)) return
        val argList = mc.args
        for (expr <- argList.exprs) {
          expr match {
            case literal@ScBooleanLiteral(_) if isArgForBooleanParam(expr, argList) &&
              addNameToArgumentsFix(literal).isDefined =>
              val message = InspectionBundle.message("name.boolean.params")
              val quickFix = new NameBooleanParametersQuickFix(message, literal)
              val descriptor = holder.getManager.createProblemDescriptor(
                expr,
                message,
                quickFix,
                ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
                isOnTheFly
              )
              holder.registerProblem(descriptor)
            case _ =>
          }
        }
      }

      def isArgForBooleanParam(expr: ScExpression, argList: ScArgumentExprList): Boolean = {
        argList.parameterOf(expr).exists(isBooleanParam)
      }

      def isBooleanParam(p: Parameter): Boolean = {
        implicit val projectContext: ProjectContext = holder.getProject

        if (p.isRepeated) false
        else {
          val typeElem = p.paramInCode.flatMap(_.typeElement)
          typeElem.exists(_.calcType.equiv(Boolean))
        }
      }

      def isSingleParamMethodCall(mc: ScMethodCall): Boolean = {
        mc.getInvokedExpr match {
          case ref: ScReferenceExpression =>
            ref.bind().exists { srr =>
              val targets = (Seq(srr.element) ++ srr.innerResolveResult.map(_.getElement)).filterBy[ScFunction]
              targets.exists(_.parameters.size == 1)
            }
          case _ => false
        }
      }

    }
  }

  def isIgnoreSingleParameter: Boolean

  def setIgnoreSingleParameter(value: Boolean)

}

object NameBooleanParametersInspectionBase {

  private def addNameToArgumentsFix(literal: ScLiteral) =
    codeInsight.intention.addNameToArgumentsFix(literal, onlyBoolean = true)

  private class NameBooleanParametersQuickFix(name: String, element: ScLiteral)
    extends AbstractFixOnPsiElement(name, element) {

    override protected def doApplyFix(elem: ScLiteral)
                                     (implicit project: Project): Unit = {
      addNameToArgumentsFix(elem).foreach(_.apply())
    }
  }

}