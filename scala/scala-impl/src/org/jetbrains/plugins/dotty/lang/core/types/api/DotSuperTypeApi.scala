package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

trait DotSuperTypeApi { this: DotSuperType =>
  def underlying: DotType = superTpe

  override def superType: DotType = ???

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedSuper = superTpe.recursiveUpdateImpl(substitutor, variance)

    if (updatedSuper eq superTpe) this
    else                          DotSuperType(symbol, updatedSuper)
  }
}
