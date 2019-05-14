package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, Stop}
import org.jetbrains.plugins.scala.lang.psi.types.{LeafType, ScalaType}

/**
 * Nikolay.Tropin
 * 11-Aug-17
 */
class Extensions[Tpe <: ScalaType](private val tp: Tpe) extends AnyVal {
  def recursiveVarianceUpdate(
    variance: Variance = Variance.Covariant
  )(update:   Update[Tpe]
  )(implicit
    updater: SubtypeUpdater[Tpe]
  ): Tpe =
    updater.recursiveUpdate(tp, variance, update)

  //allows most control on what should be done when encountering a type
  def recursiveUpdate(update: SimpleUpdate[Tpe])(implicit updater: SubtypeUpdater[Tpe]): Tpe =
    updater.noVariance.recursiveUpdate(tp, Variance.Covariant, update)

  //updates all matching subtypes recursively
  def updateRecursively(pf: PartialFunction[Tpe, Tpe])(implicit updater: SubtypeUpdater[Tpe]): Tpe =
    updater.noVariance.recursiveUpdate(tp, Variance.Covariant, SimpleUpdate(pf))

  def updateLeaves(pf: PartialFunction[LeafType, Tpe])(implicit updater: SubtypeUpdater[Tpe]): Tpe =
    updater.noVariance.recursiveUpdate(tp, Variance.Covariant, LeafSubstitution(pf))

  //invokes a function with a side-effect recursively, doesn't create any new types
  def visitRecursively(fun: Tpe => Unit)(implicit updater: SubtypeUpdater[Tpe]): Tpe =
    updater.traverser.recursiveUpdate(tp, Variance.Covariant, foreachSubtypeUpdate(fun))

  def subtypeExists(predicate: Tpe => Boolean)(implicit updater: SubtypeUpdater[Tpe]): Boolean = {
    var found = false
    visitRecursively {
      case t if predicate(t) || found =>
        found = true
        Stop
      case _ =>
        ProcessSubtypes
    }
    found
  }

  private def foreachSubtypeUpdate(fun: Tpe => Unit): SimpleUpdate[Tpe] = tpe => {
    fun(tpe)
    ProcessSubtypes
  }
}
