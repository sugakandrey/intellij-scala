package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

trait DotNamedTypeApi { this: DotNamedType =>
  def withPrefix(prefix: DotType): DotNamedType

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val updatedPrefix = prefix.recursiveUpdateImpl(substitutor, variance)
    if (prefix eq updatedPrefix) this
    else withPrefix(updatedPrefix)
  }
}
