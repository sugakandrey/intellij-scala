package org.jetbrains.plugins.scala.lang

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}

package object typeInference {
  /** An alias for old style Parameter usage, to avoid touching Scala2 related code */
  type Parameter      = ParameterT[ScType]
  type DotParameter   = ParameterT[DotType]
  type ScalaParameter = ParameterT[_ <: ScalaType]

  type TypeParameter      = TypeParameterT[ScType]
  type DotTypeParameter   = TypeParameterT[DotType]
  type ScalaTypeParameter = TypeParameterT[_ <: ScalaType]

  val Parameter: ParameterT.type = ParameterT
  val TypeParameter: TypeParameterT.type = TypeParameterT

  object Scala2Parameter {
    def unapply(parameter: ParameterT[_]): Option[Parameter] = parameter.scala2Parameter
  }

  object DottyParameter {
    def unapply(parameter: ParameterT[_]): Option[DotParameter] = parameter.dottyParameter
  }

  implicit class ParameterSeqExt(private val parameters: Seq[ScalaParameter]) extends AnyVal {
    def scala2Parameters: Seq[Parameter] = parameters.collect { case Scala2Parameter(p) => p }
    def dotParameters: Seq[DotParameter] = parameters.collect { case DottyParameter(p)  => p }
  }
}
