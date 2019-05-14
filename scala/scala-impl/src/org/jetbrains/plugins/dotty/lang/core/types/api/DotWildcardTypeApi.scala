package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotWildcardTypeApi { self: DotWildcardType =>
  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedBounds = bounds.map(_.recursiveUpdateImpl(substitutor, variance))
    if (bounds == updatedBounds) this
    else DotWildcardType(updatedBounds)
  }
}
