package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance._
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor

private[core] trait DotAppliedTypeApi { this: DotAppliedType =>
  def underlying: DotType = tycon

  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    val variances    = tycon.typeParams.map(_.variance)
    val updatedTycon = tycon.recursiveUpdateImpl(substitutor, variance)

    val updatedTypeArgs = typeArgs.smartMapWithIndex {
      case (ta, i) =>
        val v = if (i < variances.length) variances(i) else Invariant
        ta.recursiveUpdateImpl(substitutor, v * variance)
    }

    if ((tycon eq updatedTycon) && (typeArgs eq updatedTypeArgs)) this
    else DotAppliedType(updatedTycon, updatedTypeArgs)
  }

  override def superType: DotType = tycon match {
    case _: DotHKTypeLambda                      => typeSystem.Any
    case DotTypeRef(_, symbol) if symbol.isClass => tycon
    case proxy: DotProxyType                     => proxy.superType.applyIfParameterized(typeArgs)
    case _                                       => typeSystem.Any

  }
}
