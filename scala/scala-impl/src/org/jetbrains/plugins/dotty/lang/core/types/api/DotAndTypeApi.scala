package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotAndTypeApi { self: DotAndType =>
  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val lhsUpdated = lhs.recursiveUpdateImpl(substitutor, variance)
    val rhsUpdated = rhs.recursiveUpdateImpl(substitutor, variance)

    if ((lhs eq lhsUpdated) && (rhs eq rhsUpdated)) this
    else                                            DotAndType(lhsUpdated, rhsUpdated)
  }
}
