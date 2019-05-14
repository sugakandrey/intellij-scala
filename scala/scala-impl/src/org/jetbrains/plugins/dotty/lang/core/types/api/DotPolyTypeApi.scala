package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotPolyTypeApi { self: DotPolyType =>
  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedInnerTpe = innerTpe.recursiveUpdateImpl(substitutor, variance)
    val updatedTParams  = tparams.smartMap(_.update(substitutor, -variance))
    if ((innerTpe eq updatedInnerTpe) && (tparams eq updatedTParams)) this
    else DotPolyType(updatedTParams, updatedInnerTpe)
  }
}
