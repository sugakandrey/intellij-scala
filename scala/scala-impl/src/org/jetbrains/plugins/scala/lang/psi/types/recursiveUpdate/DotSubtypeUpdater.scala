package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.dotty.lang.core.types._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance.Invariant
import org.jetbrains.plugins.scala.lang.typeInference.DotParameter

final case class DotSubtypeUpdater(needVariance: Boolean, needUpdate: Boolean)
    extends SubtypeUpdater[DotType] {

  override def withVariance: SubtypeUpdater[DotType] = copy(needVariance = true, needUpdate = true)
  override def noVariance: SubtypeUpdater[DotType] = copy(needVariance = false, needUpdate = true)
  override def traverser: SubtypeUpdater[DotType] = copy(needVariance = false, needUpdate = false)

  private def updateDotPolyType(
    poly:        DotPolyType,
    variance:    Variance,
    substitutor: DotSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): DotPolyType = {
    import poly._
    val updatedInnerTpe = substitutor.recursiveUpdateImpl(innerTpe, variance)
    val updatedTParams = tparams.smartMap(updateTypeParameter(_, substitutor, -variance))

    if ((innerTpe eq updatedInnerTpe) && (tparams eq updatedTParams)) poly
    else DotPolyType(updatedTParams, updatedInnerTpe)
  }

  private def updateMethodType(
    method:      DotMethodType,
    variance:    Variance,
    substitutor: DotSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): DotMethodType = {
    import method._

    def updateParameterType(tp: DotType) =
      substitutor.recursiveUpdateImpl(tp, -variance, isLazySubtype = true)

    def updateParameter(p: DotParameter): DotParameter = {
      val updatedType = updateParameterType(p.paramType)
      val updatedExpectedType = updateParameterType(p.expectedType)
      val updatedDefault = p.defaultType.map(updateParameterType)

      if ((updatedType eq p.paramType) && (updatedExpectedType eq p.expectedType) && updatedDefault == p.defaultType)
        p
      else
        p.copy(
          paramType = updateParameterType(p.paramType),
          expectedType = updateParameterType(p.expectedType),
          defaultType = p.defaultType.map(updateParameterType)
        )
    }

    val updatedResTpe = substitutor.recursiveUpdateImpl(resTpe, variance)
    val updatedParams = params.smartMap(updateParameter)

    if ((resTpe eq updatedResTpe) && (params eq updatedParams)) method
    else DotMethodType(updatedParams, updatedResTpe)
  }

  private def updateAliasType(
    alias:       DotAliasType,
    variance:    Variance,
    substitutor: DotSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): DotAliasType = {
    import alias._
    val updated = substitutor.recursiveUpdateImpl(aliased, variance)

    if (aliased eq updated) alias
    else DotAliasType(updated)
  }

  private def updateTypeBounds(
    bounds:      DotTypeBounds,
    variance:    Variance,
    substitutor: DotSubstitutor
  )(implicit
    visited: Set[ScalaType]
  ): DotTypeBounds = {
    import bounds._
    val loUpdated = substitutor.recursiveUpdateImpl(lo, -variance)
    val hiUpdated = substitutor.recursiveUpdateImpl(hi, variance)

    if ((lo eq loUpdated) && (hi eq hiUpdated)) bounds
    else DotTypeBounds(loUpdated, hiUpdated)
  }

  private def updateOrType(
    orType:      DotOrType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotOrType = {
    import orType._
    val lhsUpdated = substitutor.recursiveUpdateImpl(lhs, variance)
    val rhsUpdated = substitutor.recursiveUpdateImpl(rhs, variance)

    if ((lhs eq lhsUpdated) && (rhs eq rhsUpdated)) orType
    else DotOrType(lhsUpdated, rhsUpdated)
  }

  private def updateAndType(
    andType:     DotAndType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotAndType = {
    import andType._
    val lhsUpdated = substitutor.recursiveUpdateImpl(lhs, variance)
    val rhsUpdated = substitutor.recursiveUpdateImpl(rhs, variance)

    if ((lhs eq lhsUpdated) && (rhs eq rhsUpdated)) andType
    else DotAndType(lhsUpdated, rhsUpdated)
  }

  private def updateAppliedType(
    appliedType: DotAppliedType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotAppliedType = {
    import appliedType._
    val variances    =
      if (needVariance) tycon.typeParams.map(_.variance)
      else Seq.empty

    val updatedTycon = substitutor.recursiveUpdateImpl(tycon, variance)

    val updatedTypeArgs = typeArgs.smartMapWithIndex {
      case (ta, i) =>
        val v = if (i < variances.length) variances(i) else Invariant
        substitutor.recursiveUpdateImpl(ta, v * variance)
    }

    if ((tycon eq updatedTycon) && (typeArgs eq updatedTypeArgs)) appliedType
    else DotAppliedType(updatedTycon, updatedTypeArgs)
  }

  private def updateTypeLambda(
    lambda:      DotHKTypeLambda,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotType = {
    import lambda._
    val updatedResTpe  = substitutor.recursiveUpdateImpl(resTpe, variance)
    val updatedTParams = tparams.smartMap(updateTypeParameter(_, substitutor, -variance))

    if ((resTpe eq updatedResTpe) && (tparams eq updatedTParams)) lambda
    else DotHKTypeLambda(updatedTParams, updatedResTpe)
  }

  private def updateExprType(
    exprType:    DotExprType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotExprType = {
    import exprType._
    val updated = substitutor.recursiveUpdateImpl(resTpe, variance)

    if (resTpe eq updated) exprType
    else DotExprType(updated)
  }

  private def updateMatchType(
    matchType:   DotMatchType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotMatchType = {
    import matchType._

    // TODO: variance
    val updatedScrutinee = substitutor.recursiveUpdateImpl(scrutinee, variance)
    val updatedBound = substitutor.recursiveUpdateImpl(bound, variance)
    val updatedCases = cases.smartMap(substitutor.recursiveUpdateImpl(_, variance))

    if ((scrutinee eq updatedScrutinee) && (bound eq updatedBound) && (cases eq updatedCases))
      matchType
    else DotMatchType(updatedScrutinee, updatedBound, updatedCases)
  }

  private def updateNamedType(
    namedType:   DotNamedType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotNamedType = {
    import namedType._

    val updatedPrefix = substitutor.recursiveUpdateImpl(prefix, variance)
    if (prefix eq updatedPrefix) namedType
    else withPrefix(updatedPrefix)
  }

  private def updateWildcardType(
    wildcardType: DotWildcardType,
    variance:     Variance,
    subtstitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotWildcardType = {
    import wildcardType._

    val updatedBounds = bounds.map(subtstitutor.recursiveUpdateImpl(_, variance))
    if (bounds == updatedBounds) wildcardType
    else DotWildcardType(updatedBounds)
  }

  private def updateSuperType(
    superType:   DotSuperType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotSuperType = {
    import superType.{superTpe, symbol}

    val updatedSuper = substitutor.recursiveUpdateImpl(superTpe, variance)

    if (updatedSuper eq superTpe) superType
    else                          DotSuperType(symbol, updatedSuper)
  }

  private def updateRefinedType(
    refinedType: DotRefinedType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotRefinedType = {
    import refinedType._

    val updatedParent = substitutor.recursiveUpdateImpl(parent, variance)

    //TODO: recursiveUpdate signatures in the refinement
    if (parent eq updatedParent) refinedType
    else DotRefinedType(updatedParent, refinement)
  }

  private def updateTemplateInfo(
    info:        DotTemplateInfo,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotTemplateInfo = {
    // TODO: ???
    info
  }

  override def updateSubtypes(
    dotType:     DotType,
    variance:    Variance,
    substitutor: ScSubstitutorT[DotType]
  )(implicit
    visited: Set[ScalaType]
  ): DotType = dotType match {
    case tinfo: DotTemplateInfo    => updateTemplateInfo(tinfo, variance, substitutor)
    case refined: DotRefinedType   => updateRefinedType(refined, variance, substitutor)
    case superTpe: DotSuperType    => updateSuperType(superTpe, variance, substitutor)
    case wildcard: DotWildcardType => updateWildcardType(wildcard, variance, substitutor)
    case matchTpe: DotMatchType    => updateMatchType(matchTpe, variance, substitutor)
    case expr: DotExprType         => updateExprType(expr, variance, substitutor)
    case named: DotNamedType       => updateNamedType(named, variance, substitutor)
    case and: DotAndType           => updateAndType(and, variance, substitutor)
    case or: DotOrType             => updateOrType(or, variance, substitutor)
    case applied: DotAppliedType   => updateAppliedType(applied, variance, substitutor)
    case lambda: DotHKTypeLambda   => updateTypeLambda(lambda, variance, substitutor)
    case alias: DotAliasType       => updateAliasType(alias, variance, substitutor)
    case bounds: DotTypeBounds     => updateTypeBounds(bounds, variance, substitutor)
    case method: DotMethodType     => updateMethodType(method, variance, substitutor)
    case poly: DotPolyType         => updateDotPolyType(poly, variance, substitutor)
    case leaf                      => leaf
  }
}

object DotSubtypeUpdater {
  val defaultUpdater: SubtypeUpdater[DotType] =
    DotSubtypeUpdater(needVariance = true, needUpdate = true)
}
