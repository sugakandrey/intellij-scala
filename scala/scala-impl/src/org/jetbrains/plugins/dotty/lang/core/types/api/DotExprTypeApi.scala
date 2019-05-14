package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

trait DotExprTypeApi { this: DotExprType =>
  def underlying: DotType = resTpe

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updated = resTpe.recursiveUpdateImpl(substitutor, variance)

    if (resTpe eq updated) this
    else                   DotExprType(updated)
  }
}
