package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotHKTypeLambdaApi { self: DotHKTypeLambda =>
  def underlying: DotType = resTpe

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedResTpe  = resTpe.recursiveUpdateImpl(substitutor, variance)
    val updatedTParams = tparams.smartMap(_.update(substitutor, -variance))

    if ((resTpe eq updatedResTpe) && (tparams eq updatedTParams)) this
    else DotHKTypeLambda(updatedTParams, updatedResTpe)
  }
}
