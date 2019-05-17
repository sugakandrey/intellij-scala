package org.jetbrains.plugins.scala
package codeInsight
package hints

import java.{util => ju}

import com.intellij.codeInsight.hints
import com.intellij.lang.java.JavaLanguage
import com.intellij.psi.{PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction.CommonNames
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import org.jetbrains.plugins.scala.lang.typeInference.Parameter

import scala.annotation.tailrec
import scala.collection.JavaConverters

final class ScalaInlayParameterHintsProvider extends hints.InlayParameterHintsProvider {

  import CommonNames.GetSet
  import ScalaInlayParameterHintsProvider._

  override def getParameterHints(element: PsiElement): ju.List[hints.InlayInfo] = {
    val matchedParameters = (element match {
      case ResolveMethodCall(method) if GetSet(method.name) && !applyUpdateParameterNames.isEnabled => Seq.empty
      case call: ScMethodCall => call.matchedParameters.reverse
      case invocation: ScConstructorInvocation => invocation.matchedParameters
      case _ => Seq.empty
    }).filter {
      case (argument, _) => element.isAncestorOf(argument)
    }

    matchedParameters match {
      case Seq() => ju.Collections.emptyList()
      case _ =>
        import JavaConverters._
        parameterHints(matchedParameters).asJava
    }
  }

  override final def getHintInfo(element: PsiElement): hints.HintInfo = element match {
    case ResolveMethodCall(methodInfo(info)) => info
    case ResolveConstructorCall(methodInfo(info)) => info
    case _ => null
  }

  override final def getInlayPresentation(inlayText: String): String = inlayText

  override final def getSupportedOptions: ju.List[hints.Option] = ju.Arrays.asList(
    applyUpdateParameterNames,
    referenceParameterNames
  )

  override final def getDefaultBlackList: ju.Set[String] = ju.Collections.singleton("scala.*")

  override final def getBlackListDependencyLanguage: JavaLanguage = JavaLanguage.INSTANCE
}

object ScalaInlayParameterHintsProvider {

  import CommonNames.{Apply, Update}

  private[hints] val applyUpdateParameterNames = HintOption(s"<code>$Apply</code>, <code>$Update</code> methods", Apply, Update)
  private[hints] val referenceParameterNames = HintOption(s"non-literal expressions", "references", "names")

  private[this] object HintOption {

    def apply(nameSuffix: String, idSegments: String*): hints.Option = {
      val id = "scala" +: idSegments :+ "hint"
      new hints.Option(id.mkString("."), s"<html><body>Show for $nameSuffix</body></html>", false)
    }
  }

  private def parameterHints(matchedParameters: Seq[(ScExpression, Parameter)]) = {
    val (varargs, regular) = matchedParameters.partition {
      case (_, parameter) => parameter.isRepeated
    }

    (regular ++ varargs.headOption).collect {
      case (argument, parameter) if isNameable(argument) => (argument, parameter.name)
    }.filter {
      case (_: ScUnderscoreSection, _) => false
      case (_, name) if name.length <= 1 => false
      case (argument, _) if !referenceParameterNames.isEnabled => isUnclear(argument)
      case (ReferenceName(name, Seq()), parameterName) => name.mismatchesCamelCase(parameterName)
      case _ => true
    }.map {
      case (argument, name) =>
        val offset = argument.getTextRange.getStartOffset
        new hints.InlayInfo(s"$name ${ScalaTokenTypes.tASSIGN}", offset)
    }
  }

  private object methodInfo {

    import hints.HintInfo.MethodInfo

    def unapply(method: PsiMethod): Some[MethodInfo] = {
      val classFqn = method.containingClass match {
        case null => ""
        case clazz => s"${clazz.qualifiedName}."
      }

      val names = method.parameters.map(_.name)

      import JavaConverters._
      Some(new MethodInfo(classFqn + method.name, names.asJava))
    }
  }

  private object ResolveMethodCall {

    def unapply(call: ScMethodCall): Option[PsiMethod] =
      call.applyOrUpdateElement.collect {
        case ScalaResolveResult(method: PsiMethod, _) => method
      }.orElse {
        call.deepestInvokedExpr match {
          case ResolvesTo(method: PsiMethod) => Some(method)
          case _ => None
        }
      }
  }

  private object ResolveConstructorCall {

    def unapply(constrInvocation: ScConstructorInvocation): Option[PsiMethod] =
      constrInvocation.reference.collect {
        case ResolvesTo(method: PsiMethod) => method
      }
  }

  private[this] def isNameable(argument: ScExpression) =
    argument.getParent match {
      case list: ScArgumentExprList => !list.isBraceArgs
      case _ => false
    }

  @tailrec
  private[this] def isUnclear(expression: ScExpression): Boolean = expression match {
    case _: ScLiteral | _: ScThisReference => true
    case ScParenthesisedExpr(inner) => isUnclear(inner)
    case ScSugarCallExpr(base, _, _) => isUnclear(base)
    case _ => false
  }
}
