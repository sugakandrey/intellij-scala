package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.parser.parsing.expressions.InfixExpr
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.TypeParamIdOwner
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.TypePresentation
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter

trait DottyTypePresentation extends TypePresentation[DotType] {
  import DottyTypePresentation._

  // FIXME
  override def presentableText(
    tpe:        DotType,
    withPrefix: Boolean
  )(implicit
    context: TypePresentationContext
  ): String = canonicalText(tpe)

  // FIXME
  override def urlText(tpe: DotType): String = canonicalText(tpe)

  override def canonicalText(tpe: DotType): String = typeText(tpe)

  private def typeText(tpe: DotType): String = {
    def typeParametersText(parameters: Seq[DotTypeParameter]): String =
      if (parameters.isEmpty) ""
      else
        parameters.map { tp =>
          val builder = StringBuilder.newBuilder
          builder ++= tp.variance.toString
          builder ++= tp.name
          builder ++= typeTextRHS(DotTypeBounds(tp.lowerType, tp.upperType))
          builder.result()
        }.commaSeparated(model = Model.SquareBrackets)

    def typeTextRHS(tpe: DotType): String = tpe match {
      case DotAliasType(aliased) => s" = ${typeText(aliased)}"
      case DotTypeBounds(lo, hi) =>
        val hiText =
          if (hi.isAny) ""
          else s" <: ${typeText(hi)}"

        val loText =
          if (lo.isNothing) ""
          else s" >: ${typeText(lo)}"
        hiText + loText
      case other => typeText(other)
    }

    def typeTextRef(tpe: DotSingletonType): String = tpe match {
      case DotConstantType(value)         => value.toString
      case DotTermRef(prefix, designator) => typeTextPrefix(prefix) + designator.name
      case DotSuperType(classDenot, _)    => classDenot.name + ".super"
      case DotThisType(classDenot)        => classDenot.name + ".this"
    }

    def typeTextPrefix(tpe: DotType): String = tpe match {
      case DotNoPrefix                 => ""
      case singleton: DotSingletonType => typeText(singleton) + "."
      case other                       => typeText(other) + "#"
    }

    def refinementText(rtpe: DotRefinedType): String =
      rtpe.refinement.toString + ";"

    def infixTypeText(op: String, lhs: DotType, rhs: DotType): String = {
      val precedence = ParserUtils.priority(op)

      def componentText(dotType: DotType, assoc: Int, precedence: Int): String = {
        val infixName = dotType match {
          case _: DotAppliedType => None // FIXME
          case _: DotAndType     => Option("|")
          case _: DotOrType      => Option("&")
          case _                 => None
        }

        val needParens = infixName.exists(
          name =>
            InfixExpr.associate(name) != assoc ||
              ParserUtils.priority(name) > precedence
        )

        typeText(dotType).parenthesize(needParens)
      }

      s"${componentText(lhs, -1, precedence)} $op ${componentText(rhs, 1, precedence)}"
    }

    tpe match {
      case DotNoPrefix                     => "<noprefix>"
      case DotWildcardType(optBounds)      => optBounds.fold("?")(bound => s"(? ${typeTextRHS(bound)})")
      case DotTypeRef(prefix, designator)  => typeTextPrefix(prefix) + designator.name
      case termRef: DotTermRef             => typeTextRef(termRef) + ".type"
      case singleton: DotSingletonType     => typeText(singleton.underlying) + "( " + typeTextRef(singleton) + " )"
      case DotAppliedType(tycon, args)     => typeText(tycon) + args.map(typeText).commaSeparated(model = Model.SquareBrackets)
      case DotTypeVar(tparam)              => "NotInferred" + tparam.name + tparam.typeParamId
      case DotPolyType(tparams, methodTpe) => typeParametersText(tparams) + typeText(methodTpe)
      case DotMethodType(params, resTpe) =>
        params
          .map(param => s"${param.name}: ${typeText(param.paramType)}")
          .commaSeparated(model = Model.Parentheses) + typeText(resTpe)
      case DotPolyType(tparams, methodTpe)  => typeParametersText(tparams) + typeText(methodTpe)
      case DotHKTypeLambda(tparams, resTpe) => typeParametersText(tparams) + " => " + typeText(resTpe)
      case DotExprType(resTpe)              => "=>" + typeText(resTpe)
      case rtpe: DotRefinedType =>
        val parent :: (refined: List[DotRefinedType @unchecked]) =
          refinementChain(rtpe).reverse
        typeText(parent) + "{" + refined.map(refinementText) + "}"
      case DotMatchType(scrutinee, bound, cases) =>
        def caseText(tpe: DotType): String = tpe match {
//          case DotHKTypeLambda(tparams, resTpe) =>  FIXME: apply type lambda
          case t => "case " + typeText(t)
        }
        val casesText = cases.map(caseText).mkString("\n\t", "\n\t", "\n")
        val boundText = if (!bound.isAny) " <: " + typeText(bound) else ""
        typeText(scrutinee) + " match {" + casesText + "}" + boundText
      case DotAndType(lhs, rhs)                  => infixTypeText("&", lhs, rhs)
      case DotOrType(lhs, rhs)                   => infixTypeText("|", lhs, rhs)
      case fallback                              => fallback.toString
    }
  }

}

object DottyTypePresentation {
  private[DottyTypePresentation] def refinementChain(tpe: DotType): List[DotType] =
    tpe :: (tpe match {
      case rtpe: DotRefinedType => refinementChain(rtpe.parent)
      case _                    => Nil
    })
}
