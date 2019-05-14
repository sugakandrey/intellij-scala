package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotAliasTypeApi { self: DotAliasType =>
  override def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updated = aliased.recursiveUpdateImpl(substitutor, variance)

    if (aliased eq updated) this
    else                    DotAliasType(updated)
  }
}
