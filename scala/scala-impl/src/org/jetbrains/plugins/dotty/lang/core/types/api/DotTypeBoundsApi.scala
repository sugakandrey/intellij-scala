package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotType, DotTypeBounds}
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotTypeBoundsApi { self: DotTypeBounds =>
  def underlying: DotType = hi

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val loUpdated = lo.recursiveUpdateImpl(substitutor, -variance)
    val hiUpdated = hi.recursiveUpdateImpl(substitutor, variance)

    if ((lo eq loUpdated) && (hi eq hiUpdated)) this
    else                                        DotTypeBounds(loUpdated, hiUpdated)
  }
}
