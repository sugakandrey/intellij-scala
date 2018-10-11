package org.jetbrains.plugins.scala.actions

import java.util

import com.intellij.codeInsight.documentation.DocumentationComponent
import com.intellij.lang.ExpressionTypeProvider
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.PsiElement
import com.intellij.ui.ColorUtil
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScBindingPattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, TypePresentationContext}

import scala.collection.JavaConverters._

class ScalaExpressionTypeProvider extends ExpressionTypeProvider[ScalaPsiElement] {
  import ScalaExpressionTypeProvider._
  private[this] val unknownType = "<unknown>"

  override def getErrorHint: String = "No expression found"

  override def getExpressionsAt(elementAt: PsiElement): util.List[ScalaPsiElement] =
    elementAt.withParentsInFile.collect {
      case pattern: ScBindingPattern => pattern: ScalaPsiElement
      case typeable: ScExpression    => typeable: ScalaPsiElement
    }.toList.asJava

  override def getInformationHint(element: ScalaPsiElement): String =
    extractType(element).fold(unknownType)(_.presentableText(element))

  override def hasAdvancedInformation: Boolean = true

  override def getAdvancedInformationHint(element: ScalaPsiElement): String =
    extractType(element).fold(unknownType) { t: ScType =>
      val dealiased = t.removeAliasDefinitions()
      val widened   = t.tryExtractDesignatorSingleton

      val (expected, withoutImplicits) = element match {
        case expr: ScExpression => (expr.expectedType(), expr.getTypeWithoutImplicits().toOption)
        case _                  => (None, None)
      }

      s"""<table>
         |${makeAdvancedInformationTableRow("Original", t)}
         |${makeAdvancedInformationTableRow("Dealiased", dealiased)}
         |${makeAdvancedInformationTableRow("Widened", widened)}
         |${expected.map(makeAdvancedInformationTableRow("Expected", _))}
         |${withoutImplicits.map(makeAdvancedInformationTableRow("Without Implicits", _))}
         |</table>
     """.stripMargin
    }
}

private object ScalaExpressionTypeProvider {
  def extractType(e: PsiElement): Option[ScType] = e match {
    case ResolvedWithSubst(target, subst) => target.ofNamedElement(subst)
    case Typeable(tpe)                    => Option(tpe)
    case _                                => None
  }

  def makeAdvancedInformationTableRow(
    title:        String,
    tpe:          ScType
  )(implicit ctx: TypePresentationContext): String = {
    val titleCell = "<td align='left' valign='top' style='color:" +
      ColorUtil.toHtmlColor(DocumentationComponent.SECTION_COLOR) + "'>" +
      StringUtil.escapeXml(title) + ":</td>"

    val contentCell = s"<td>${tpe.presentableText}</td>"
    s"<tr>$titleCell$contentCell</tr>"
  }
}
