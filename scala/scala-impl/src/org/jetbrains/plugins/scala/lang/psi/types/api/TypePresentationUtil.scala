package org.jetbrains.plugins.scala.lang.psi.types.api

import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.scala.extensions.childOf
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScRefinement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAliasDeclaration, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes.{tLOWER_BOUND, tUPPER_BOUND}

object TypePresentationUtil {
  val ABSTRACT_TYPE_POSTFIX = "_"

  final case class ScTypeText(tp: ScalaType) {
    val canonicalText: String   = tp.canonicalText
    val presentableText: String = tp.presentableText
  }

  def different(t1: ScalaType, t2: ScalaType)
               (implicit context: TypePresentationContext): (String, String) = {
    val (p1, p2) = (t1.presentableText, t2.presentableText)
    if (p1 != p2) (p1, p2)
    else (t1.canonicalText.replace("_root_.", ""), t2.canonicalText.replace("_root_.", ""))
  }

  def shouldExpand(typeAlias: ScTypeAliasDefinition): Boolean = typeAlias match {
    case childOf(_, _: ScRefinement) => true
    case _ => ScalaPsiUtil.superTypeMembers(typeAlias).exists(_.isInstanceOf[ScTypeAliasDeclaration])
  }

  def withoutAliases(`type`: ScType)
                    (implicit context: TypePresentationContext): String = {
    `type`.removeAliasDefinitions(expandableOnly = true).presentableText
  }

  def upperBoundText(maybeType: TypeResult)
                    (toString: ScType => String): String =
    upperBoundText(maybeType.toOption)(toString)

  def upperBoundText(`type`: ScType)
                    (toString: ScType => String): String =
    upperBoundText(Some(`type`))(toString)

  def lowerBoundText(maybeType: TypeResult)
                    (toString: ScType => String): String =
    lowerBoundText(maybeType.toOption)(toString)

  def lowerBoundText(`type`: ScType)
                    (toString: ScType => String): String =
    lowerBoundText(Some(`type`))(toString)


  private[this] def upperBoundText(maybeType: Option[ScType])
                                  (toString: ScType => String): String =
    boundText(maybeType, tUPPER_BOUND)(_.isAny, toString)

  private[this] def lowerBoundText(maybeType: Option[ScType])
                                  (toString: ScType => String): String =
    boundText(maybeType, tLOWER_BOUND)(_.isNothing, toString)

  private[this] def boundText(maybeType: Option[ScType], bound: IElementType)
                             (predicate: ScType => Boolean, toString: ScType => String) =
    maybeType.collect {
      case t if !predicate(t) => " " + bound + " " + toString(t)
    }.getOrElse("")
}
