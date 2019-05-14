package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance.Invariant
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameterT

abstract class SubtypeUpdater[Tpe <: ScalaType](needVariance: Boolean, needUpdate: Boolean) {
  protected implicit def implicitThis: SubtypeUpdater[Tpe] = this

  /** Returns modified version of this traverser, with [[needVariance]] set to `false` */
  def noVariance: SubtypeUpdater[Tpe]

  /** Returns modified version of this traverser suitable for type inspection only */
  def traverser: SubtypeUpdater[Tpe]

  def updateSubtypes(
    scType:      Tpe,
    variance:    Variance,
    substitutor: ScSubstitutorT[Tpe]
  )(implicit
    visited: Set[Tpe]
  ): Tpe

  def updateTypeParameter(
    tparam:      TypeParameterT[Tpe],
    substitutor: ScSubstitutorT[Tpe],
    variance:    Variance = Invariant
  )(implicit
    visited: Set[Tpe]
  ): TypeParameterT[Tpe]

  final def recursiveUpdate(tpe: Tpe, variance: Variance, update: Update[Tpe]): Tpe =
    update(tpe, variance) match {
      case ReplaceWith(res) => res
      case Stop             => tpe
      case ProcessSubtypes  => updateSubtypes(tpe, variance, ScSubstitutorT(update))(Set.empty)
    }
}

object SubtypeUpdater {
  implicit class TypeParameterUpdateExt[Tpe <: ScalaType](private val typeParameter: TypeParameterT[Tpe])
      extends AnyVal {
    def update(
      substitutor: ScSubstitutorT[Tpe]
    )(implicit
      updater: SubtypeUpdater[Tpe],
      visited: Set[Tpe] = Set.empty
    ): TypeParameterT[Tpe] = updater.updateTypeParameter(typeParameter, substitutor)
  }

  implicit val scUpdater: SubtypeUpdater[ScType] = ScSubtypeUpdater.defaultUpdater
  implicit val dotUpdater: SubtypeUpdater[DotType] = ???
  implicit val anyTpeUpdater: SubtypeUpdater[ScalaType] = ???
}
