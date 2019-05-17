package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition, ScMacroDefinition, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScTypeParam, ScTypeParamClause}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeBoundsOwner
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.result.{TypeResult, TypeResultT, Typeable}
import org.jetbrains.plugins.scala.project.ProjectContextOwner

trait ScalaTyper extends api.Typer[ScType] {
  this: TypeSystem[ScType] =>

  override def tpe(target: Typeable): TypeResult = target.`type`()

  @annotation.tailrec
  private def extractBound(owner: ScTypeBoundsOwner, tpe: ScType, isLower: Boolean): ScType =
    owner match {
      case tparam: ScTypeParam =>
        tparam.typeParametersClause match {
          case Some(pclause: ScTypeParamClause) =>
            val tparams = pclause.typeParameters
            tpe match {
              case ParameterizedType(des, params) =>
                if (params.length == tparams.length && params.forall(
                      _.isInstanceOf[TypeParameterType]
                    ) &&
                    params.map(_.asInstanceOf[TypeParameterType].psiTypeParameter) == tparams) {
                  des
                } else {
                  //here we should actually construct existential type for partial application
                  tpe
                }
              case t: ScType =>
                t.isAliasType match {
                  case Some(AliasType(alias: ScTypeAliasDefinition, Right(lower), _)) if isLower =>
                    extractBound(alias, lower, isLower)
                  case Some(AliasType(alias: ScTypeAliasDefinition, _, Right(upper))) if !isLower =>
                    extractBound(alias, upper, isLower)
                  case None => t
                }
            }
          case _ => tpe
        }
    }

  override def extractTypeBound(owner: ScTypeBoundsOwner, isLower: Boolean): TypeResult = {
    val maybeTe = if (isLower) owner.lowerTypeElement else owner.upperTypeElement

    maybeTe match {
      case None     => Right(if (isLower) Nothing else Any)
      case Some(te) => te.`type`().map(extractBound(owner, _, isLower))
    }
  }

  override protected def widenDefTpe(tpe: ScType): ScType = ScLiteralType.widenRecursive(tpe)
}
