package org.jetbrains.plugins.scala.lang.typeInference

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScTypeParam, TypeParamIdOwner}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.light.scala.DummyLightTypeParam
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{Bivariant, Covariant, Invariant, Nothing, TypeParameterType, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, Stop}
import org.jetbrains.plugins.scala.lang.psi.types.result._

/**
  * Class representing type parameters in our type system. Can be constructed from psi.
  *
  * lowerType and upperType sometimes should be lazy, see SCL-7216
  */
sealed trait TypeParameterT[Tpe <: ScalaType] {
  val psiTypeParameter: PsiTypeParameter
  val typeParameters: Seq[TypeParameterT[Tpe]]

  def lowerType: Tpe
  def upperType: Tpe

  // TODO: make psiTypeParameteer optional and move corresponding
  //       method implementations to PhysicalTypeParameterT
  def variance: Variance = psiTypeParameter.asOptionOf[ScTypeParam].fold(Invariant)(_.variance)

  def name: String = psiTypeParameter.name

  // TODO: change signature
  def varianceInType(tpe: ScType): Variance

  def isInvariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(t =>
    !t.isCovariant && !t.isContravariant
  )

  def isCovariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(_.isCovariant)
  def isContravariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(_.isContravariant)
}

object TypeParameterT {
  sealed trait Scala2TypeParameter extends TypeParameterT[ScType] {
    /**see [[scala.reflect.internal.Variances.varianceInType]]*/
    def varianceInType(scType: ScType): Variance = {
      val thisId = this.typeParamId
      var result: Variance = Bivariant

      scType.recursiveVarianceUpdate(Covariant) {
        case (TypeParameterType(tp), variance: Variance) if thisId == tp.typeParamId =>
          result = result & variance
          Stop
        case _ =>
          ProcessSubtypes
      }
      result
    }
  }

  def apply(typeParameter: PsiTypeParameter): TypeParameter = typeParameter match {
    case typeParam: ScTypeParam => ScalaTypeParameter(typeParam)
    case _                      => JavaTypeParameter(typeParameter)
  }

  def apply(psiTypeParameter: PsiTypeParameter,
            typeParameters: Seq[TypeParameter],
            lType: ScType,
            uType: ScType): TypeParameter = StrictTp(psiTypeParameter, typeParameters, lType, uType)

  def light(name: String, typeParameters: Seq[TypeParameter], lower: ScType, upper: ScType): TypeParameter =
    LightTypeParameter(name, typeParameters, lower, upper)

  def unapply(tp: TypeParameter): Option[(PsiTypeParameter, Seq[TypeParameter], ScType, ScType)] =
    Some(tp.psiTypeParameter, tp.typeParameters, tp.lowerType, tp.upperType)

  def javaPsiTypeParameterUpperType(typeParameter: PsiTypeParameter): ScType = {
    val manager = ScalaPsiManager.instance(typeParameter.getProject)
    manager.javaPsiTypeParameterUpperType(typeParameter)
  }

  private case class StrictTp(psiTypeParameter: PsiTypeParameter,
                              typeParameters: Seq[TypeParameter],
                              override val lowerType: ScType,
                              override val upperType: ScType) extends Scala2TypeParameter

  private case class ScalaTypeParameter(psiTypeParameter: ScTypeParam) extends Scala2TypeParameter {
    override val typeParameters: Seq[TypeParameter] = psiTypeParameter.typeParameters.map(ScalaTypeParameter)

    override def lowerType: ScType = psiTypeParameter.lowerBound.getOrNothing

    override def upperType: ScType = psiTypeParameter.upperBound.getOrAny
  }

  private case class JavaTypeParameter(psiTypeParameter: PsiTypeParameter) extends Scala2TypeParameter {
    override val typeParameters: Seq[TypeParameter] = Seq.empty

    override def lowerType: ScType = Nothing(psiTypeParameter.getProject)

    override def upperType: ScType = javaPsiTypeParameterUpperType(psiTypeParameter)
  }

  private case class LightTypeParameter(override val name: String,
                                        typeParameters: Seq[TypeParameter],
                                        override val lowerType: ScType,
                                        override val upperType: ScType) extends Scala2TypeParameter {

    override val psiTypeParameter: PsiTypeParameter = new DummyLightTypeParam(name)(lowerType.projectContext)
  }
}