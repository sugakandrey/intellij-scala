package org.jetbrains.plugins.scala.lang.typeInference

import com.intellij.psi.PsiParameter
import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.extensions.PsiParameterExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.result._

import scala.reflect.runtime.universe._

/**
 * Generalized parameter, to be used by type inference related subsystems.
 * ParameterTized by the type of the underlying types, so as to make it impossible
 * to accidentally mix scala2/dotty parameters (e.g. inside a method type).
 */
final case class ParameterT[Tpe <: ScalaType : TypeTag](
  name:           String,
  deprecatedName: Option[String],
  paramType:      Tpe,
  expectedType:   Tpe,
  isDefault:      Boolean = false,
  isRepeated:     Boolean = false,
  isByName:       Boolean = false,
  index:          Int = -1,
  psiParam:       Option[PsiParameter] = None,
  defaultType:    Option[Tpe] = None
) {
  def paramInCode: Option[ScParameter] = psiParam.collect {
    case parameter: ScParameter => parameter
  }

  def nameInCode: Option[String] = psiParam.map(_.getName)

  def isImplicit: Boolean = paramInCode.exists(_.isImplicitParameter)

  def dottyParameter: Option[ParameterT[DotType]] = typeOf[Tpe] match {
    case t if t =:= typeOf[DotType] => Option(this.asInstanceOf[ParameterT[DotType]])
    case _                          => None
  }

  def scala2Parameter: Option[Parameter] = typeOf[Tpe] match {
    case t if t =:= typeOf[ScType] => Option(this.asInstanceOf[ParameterT[ScType]])
    case _                         => None
  }
}

object ParameterT {
  def apply[Tpe <: ScalaType: TypeTag](paramType: Tpe, isRepeated: Boolean, index: Int): ParameterT[Tpe] =
    ParameterT(
      name           = "",
      deprecatedName = None,
      paramType      = paramType,
      expectedType   = paramType,
      isDefault      = false,
      isRepeated     = isRepeated,
      isByName       = false,
      index          = index
    )


  def apply(parameter: PsiParameter): ParameterT[ScType] = parameter match {
     case scParameterT: ScParameter =>
      val tpe = scParameterT.`type`().getOrNothing
       ParameterT(
         name           = scParameterT.name,
         deprecatedName = scParameterT.deprecatedName,
         paramType      = tpe,
         expectedType   = tpe,
         isDefault      = scParameterT.isDefaultParam,
         isRepeated     = scParameterT.isRepeatedParameter,
         isByName       = scParameterT.isCallByNameParameter,
         index          = scParameterT.index,
         psiParam       = Some(scParameterT),
         defaultType    = scParameterT.getDefaultExpression.flatMap(_.`type`().toOption)
       )
    case _ =>
      val tpe = parameter.paramType(exact = false)
      ParameterT(
        name           = parameter.getName,
        deprecatedName = None,
        paramType      = tpe,
        expectedType   = tpe,
        isRepeated     = parameter.isVarArgs,
        index          = parameter.index,
        psiParam       = Some(parameter)
      )
  }
}
