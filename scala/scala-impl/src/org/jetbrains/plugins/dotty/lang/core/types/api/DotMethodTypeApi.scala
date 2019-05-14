package org.jetbrains.plugins.dotty.lang.core.types
package api

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.DotSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.DotParameter

private[core] trait DotMethodTypeApi { self: DotMethodType =>
  def updateSubtypes(
    substitutor: DotSubstitutor,
    variance:    Variance
  )(implicit
    visited: Set[DotType]
  ): DotType = {
    def updateParameterType(tp: DotType) = tp.recursiveUpdateImpl(substitutor, -variance, isLazySubtype = true)

    def updateParameter(p: DotParameter): DotParameter = {
      val updatedType = updateParameterType(p.paramType)
      val updatedExpectedType = updateParameterType(p.expectedType)
      val updatedDefault = p.defaultType.map(updateParameterType)

      if ((updatedType eq p.paramType) && (updatedExpectedType eq p.expectedType) && updatedDefault == p.defaultType) p
      else p.copy(
        paramType    = updateParameterType(p.paramType),
        expectedType = updateParameterType(p.expectedType),
        defaultType  = p.defaultType.map(updateParameterType)
      )
    }

    val updatedResTpe = resTpe.recursiveUpdateImpl(substitutor, variance)
    val updatedParams = params.smartMap(updateParameter)

    if ((resTpe eq updatedResTpe) && (params eq updatedParams)) this
    else DotMethodType(updatedParams, updatedResTpe)
  }
}
