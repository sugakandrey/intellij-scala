package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotRefinedType, DotType}
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotRefinedTypeApi { self: DotRefinedType =>
  def underlying: DotType = parent

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedParent = parent.recursiveUpdateImpl(substitutor, variance)

    //TODO: recursiveUpdate signatures in the refinement
    if (parent eq updatedParent) this
    else DotRefinedType(updatedParent, refinement)
  }
}
