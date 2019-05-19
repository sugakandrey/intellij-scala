package org.jetbrains.plugins.scala.lang.typeInference

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeParamSymbol
import org.jetbrains.plugins.dotty.lang.core.types.DottyDefinitions.HKAny
import org.jetbrains.plugins.dotty.lang.core.types.{DotType, DotTypeRef}
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScTypeParam, TypeParamIdOwner}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.light.scala.DummyLightTypeParam
import org.jetbrains.plugins.scala.lang.psi.types.api.{Bivariant, Covariant, Invariant, Nothing, TypeParameterType, TypeSystem, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, Stop}
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}
import org.jetbrains.plugins.scala.project.ProjectContext

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

  /**see [[scala.reflect.internal.Variances.varianceInType]]*/
  def varianceInType(tpe: Tpe): Variance = {
    import TypeParameterT.referencesTypeParam

    var result = Bivariant

    tpe.recursiveVarianceUpdate(Covariant) {
      case (tp, variance) if referencesTypeParam(tp, this) => result &= variance; Stop
      case _                                               => ProcessSubtypes
    }

    result
  }

  def isInvariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(t =>
    !t.isCovariant && !t.isContravariant
  )

  def isCovariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(_.isCovariant)
  def isContravariant: Boolean = psiTypeParameter.asOptionOf[ScTypeParam].exists(_.isContravariant)
}

object TypeParameterT {
  private[TypeParameterT] def referencesTypeParam(
    tpe:    ScalaType,
    target: TypeParameterT[_ <: ScalaType]
  ): Boolean = {
    val paramId = target.typeParamId

    tpe match {
      case TypeParameterType(tp)              => tp.typeParamId == paramId
      case DotTypeRef(_, TypeParamSymbol(tp)) => tp.typeParamId == paramId
      case _                                  => false
    }
  }

  final case class DotTypeParameter(override val psiTypeParameter: ScTypeParam)(private implicit val ts: TypeSystem[DotType])
      extends TypeParameterT[DotType] {

    override val typeParameters: Seq[TypeParameterT[DotType]] = psiTypeParameter.typeParameters.map(dot)

    override def lowerType: DotType =
      ts.extractTypeBound(psiTypeParameter, isLower = true)
        .getOrElse(HKAny(typeParameters))

    override def upperType: DotType =
      ts.extractTypeBound(psiTypeParameter, isLower = false).getOrElse(ts.Nothing)
  }

  def dot(psi: ScTypeParam)(implicit ts: TypeSystem[DotType]): TypeParameterT[DotType] =
    DotTypeParameter(psi)

  def apply(typeParameter: PsiTypeParameter): TypeParameter = typeParameter match {
    case typeParam: ScTypeParam => ScalaTypeParameter(typeParam)
    case _                      => JavaTypeParameter(typeParameter)
  }

  def apply[Tpe <: ScalaType](
    psiTypeParameter: PsiTypeParameter,
    typeParameters:   Seq[TypeParameterT[Tpe]],
    lType:            Tpe,
    uType:            Tpe
  ): TypeParameterT[Tpe] = StrictTp(psiTypeParameter, typeParameters, lType, uType)

  def light[Tpe <: ScalaType](
    name:           String,
    typeParameters: Seq[TypeParameterT[Tpe]],
    lower:          Tpe,
    upper:          Tpe,
    variance:       Variance = Invariant
  )(implicit
    ctx: ProjectContext
  ): TypeParameterT[Tpe] =
    LightTypeParameter(name, typeParameters, lower, upper, variance)

  def unapply(tp: TypeParameter): Option[(PsiTypeParameter, Seq[TypeParameter], ScType, ScType)] =
    Some(tp.psiTypeParameter, tp.typeParameters, tp.lowerType, tp.upperType)

  def javaPsiTypeParameterUpperType(typeParameter: PsiTypeParameter): ScType = {
    val manager = ScalaPsiManager.instance(typeParameter.getProject)
    manager.javaPsiTypeParameterUpperType(typeParameter)
  }

  private case class StrictTp[Tpe <: ScalaType](
    psiTypeParameter:       PsiTypeParameter,
    typeParameters:         Seq[TypeParameterT[Tpe]],
    override val lowerType: Tpe,
    override val upperType: Tpe
  ) extends TypeParameterT[Tpe]

  private case class ScalaTypeParameter(psiTypeParameter: ScTypeParam) extends TypeParameterT[ScType] {
    private[this] implicit val project: ProjectContext = psiTypeParameter

    override val typeParameters: Seq[TypeParameter] =
      psiTypeParameter.typeParameters.map(ScalaTypeParameter)

    override def lowerType: ScType = psiTypeParameter.lowerBound.getOrNothing
    override def upperType: ScType = psiTypeParameter.upperBound.getOrAny
  }

  private case class JavaTypeParameter(psiTypeParameter: PsiTypeParameter) extends TypeParameterT[ScType] {
    override val typeParameters: Seq[TypeParameter] = Seq.empty

    override def lowerType: ScType = Nothing(psiTypeParameter.getProject)
    override def upperType: ScType = javaPsiTypeParameterUpperType(psiTypeParameter)
  }

  private case class LightTypeParameter[Tpe <: ScalaType](
    override val name:           String,
    override val typeParameters: Seq[TypeParameterT[Tpe]],
    override val lowerType:      Tpe,
    override val upperType:      Tpe,
    override val variance:       Variance = Invariant
  )(implicit
    ctx: ProjectContext
  ) extends TypeParameterT[Tpe] {
    override val psiTypeParameter: PsiTypeParameter = new DummyLightTypeParam(name)
  }
}