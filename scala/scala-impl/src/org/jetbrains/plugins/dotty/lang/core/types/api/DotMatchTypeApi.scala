package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotMatchTypeApi { this: DotMatchType =>
  def underlying: DotType = bound

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    // TODO: variance
    val updatedScrutinee = scrutinee.recursiveUpdateImpl(substitutor, variance)
    val updatedBound = bound.recursiveUpdateImpl(substitutor, variance)
    val updatedCases = cases.smartMap(_.recursiveUpdateImpl(substitutor, variance))

    if ((scrutinee eq updatedScrutinee) && (bound eq updatedBound) && (cases eq updatedCases)) this
    else DotMatchType(updatedScrutinee, updatedBound, updatedCases)
  }
}
