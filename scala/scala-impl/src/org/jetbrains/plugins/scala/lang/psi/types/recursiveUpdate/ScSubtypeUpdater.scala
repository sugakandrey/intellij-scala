package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.extensions.SeqExt
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScProjectionType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Contravariant, Covariant, Invariant, JavaArrayType, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.{ScMethodType, ScTypePolymorphicType}
import org.jetbrains.plugins.scala.lang.psi.types.{ScCompoundType, ScExistentialArgument, ScExistentialType, ScParameterizedType, ScType, ScalaType, TermSignature, TypeAliasSignature}
import org.jetbrains.plugins.scala.lang.typeInference.{Parameter, TypeParameter}

final case class ScSubtypeUpdater private (needVariance: Boolean, needUpdate: Boolean)
    extends SubtypeUpdater[ScType] {

  override def withVariance: SubtypeUpdater[ScType] = copy(needVariance = true, needUpdate = true)
  override def noVariance: SubtypeUpdater[ScType]   = copy(needVariance = false, needUpdate = true)
  override def traverser: SubtypeUpdater[ScType]    = copy(needVariance = false, needUpdate = false)

  def updateCompoundType(
    ct:          ScCompoundType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType = {

    val updSignatureMap = ct.signatureMap.map {
      case (s: TermSignature, tp) =>
        val tParams = s.typeParams.map(updateTypeParameter(_, substitutor, Invariant))
        val paramTypes = s.substitutedTypes.map(
          _.map(f => () => substitutor.recursiveUpdateImpl(f(), variance, isLazySubtype = true))
        )
        val updSignature = new TermSignature(
          s.name,
          paramTypes,
          tParams,
          s.substitutor.followed(substitutor),
          s.namedElement,
          s.hasRepeatedParam
        )
        (updSignature, substitutor.recursiveUpdateImpl(tp, Covariant))
    }

    val updatedTypes = ct.typesMap.map {
      case (s, ta) =>
        val substTps: Seq[TypeParameter] = ta.typeParams.map(updateTypeParameter(_, substitutor))
        val substLower: ScType = substitutor.recursiveUpdateImpl(ta.lowerBound)
        val substUpper: ScType = substitutor.recursiveUpdateImpl(ta.upperBound)
        val combinedSubstitutor: ScSubstitutor = ta.substitutor.followed(substitutor)

        (
          s,
          TypeAliasSignature(
            ta.typeAlias,
            ta.name,
            substTps,
            substLower,
            substUpper,
            ta.isDefinition,
            combinedSubstitutor
          )
        )
    }
    val updatedComponents = ct.components.smartMap(substitutor.recursiveUpdateImpl(_, variance))

    ScCompoundType(updatedComponents, updSignatureMap, updatedTypes)(ct.projectContext)
  }

  def updateExistentialArg(
    exArg:       ScExistentialArgument,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType =
    exArg.copyWithBounds(
      substitutor.recursiveUpdateImpl(exArg.lower, Contravariant, exArg.isLazy),
      substitutor.recursiveUpdateImpl(exArg.upper, Covariant, exArg.isLazy)
    )

  def updateExistentialType(
    exType:      ScExistentialType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType = {
    val quantified = exType.quantified
    val updatedQ = substitutor.recursiveUpdateImpl(quantified, variance)

    if (!needUpdate || (updatedQ eq quantified)) exType
    else ScExistentialType(updatedQ)
  }

  def updateParameterizedType(
    pt:          ScParameterizedType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType = {

    val designator = pt.designator
    val typeArguments = pt.typeArguments
    val typeParameterVariances =
      if (!needVariance) Seq.empty
      else
        designator.extractDesignated(expandAliases = false) match {
          case Some(n: ScTypeParametersOwner) => n.typeParameters.map(_.variance)
          case _                              => Seq.empty
        }
    val newDesignator = substitutor.recursiveUpdateImpl(designator, variance)
    val newTypeArgs = typeArguments.smartMapWithIndex {
      case (ta, i) =>
        val v = if (i < typeParameterVariances.length) typeParameterVariances(i) else Invariant
        substitutor.recursiveUpdateImpl(ta, v * variance)
    }

    if (!needUpdate || (newDesignator eq designator) && (newTypeArgs eq typeArguments)) pt
    else ScParameterizedType(newDesignator, newTypeArgs)
  }

  def updateJavaArrayType(
    arrType:     JavaArrayType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType =
    JavaArrayType(substitutor.recursiveUpdateImpl(arrType.argument, Invariant))

  def updateProjectionType(
    pt:          ScProjectionType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType = {

    val projected = pt.projected
    val updatedType = substitutor.recursiveUpdateImpl(projected, Covariant)

    if (!needUpdate || (updatedType eq projected)) pt
    else ScProjectionType(updatedType, pt.element)
  }

  def updateMethodType(
    mt:          ScMethodType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType = {

    def updateParameterType(tp: ScType) =
      substitutor.recursiveUpdateImpl(tp, -variance, isLazySubtype = true)

    def updateParameter(p: Parameter): Parameter = p.copy(
      paramType = updateParameterType(p.paramType),
      expectedType = updateParameterType(p.expectedType),
      defaultType = p.defaultType.map(updateParameterType)
    )

    ScMethodType(
      substitutor.recursiveUpdateImpl(mt.result, variance),
      mt.params.map(updateParameter),
      mt.isImplicit
    )(mt.elementScope)
  }

  def updateTypePolymorphicType(
    tpt:         ScTypePolymorphicType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType =
    ScTypePolymorphicType(
      substitutor.recursiveUpdateImpl(tpt.internalType, variance),
      tpt.typeParameters.map(updateTypeParameter(_, substitutor, -variance))
    )

  override def updateSubtypes(
    scType:      ScType,
    variance:    Variance,
    substitutor: ScSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): ScType =
    scType match {
      case t: ScCompoundType        => updateCompoundType(t, variance, substitutor)
      case t: ScExistentialArgument => updateExistentialArg(t, variance, substitutor)
      case t: ScExistentialType     => updateExistentialType(t, variance, substitutor)
      case t: ScParameterizedType   => updateParameterizedType(t, variance, substitutor)
      case t: JavaArrayType         => updateJavaArrayType(t, variance, substitutor)
      case t: ScProjectionType      => updateProjectionType(t, variance, substitutor)
      case t: ScMethodType          => updateMethodType(t, variance, substitutor)
      case t: ScTypePolymorphicType => updateTypePolymorphicType(t, variance, substitutor)
      case leaf                     => leaf
    }
}

object ScSubtypeUpdater {
  val defaultUpdater: SubtypeUpdater[ScType] = new ScSubtypeUpdater(true, true)
}
