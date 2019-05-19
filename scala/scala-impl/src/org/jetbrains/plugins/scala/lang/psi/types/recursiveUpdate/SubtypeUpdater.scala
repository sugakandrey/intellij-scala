package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance.Invariant
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{
  ProcessSubtypes,
  ReplaceWith,
  Stop
}
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameterT
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameter

trait SubtypeUpdater[Tpe <: ScalaType] {
  protected implicit def implicitThis: SubtypeUpdater[Tpe] = this

  /** Returns an instance of [[SubtypeUpdater]]
   * suitable for full-blown updates with variance information.
   */
  def withVariance: SubtypeUpdater[Tpe]

  /** Returns an instance of [[SubtypeUpdater]]
   * suitable for updates without variance information.
   */
  def noVariance: SubtypeUpdater[Tpe]

  /** Returns and instance of [[SubtypeUpdater]]
   * suitable for type inspection only (i.e. no mofication of variance info)
   */
  def traverser: SubtypeUpdater[Tpe]

  def updateSubtypes(
    scType:      Tpe,
    variance:    Variance,
    substitutor: ScSubstitutorT[Tpe]
  )(implicit
    visited: Set[ScalaType]
  ): Tpe

  def updateTypeParameter(
    tparam:      TypeParameterT[Tpe],
    substitutor: ScSubstitutorT[Tpe],
    variance:    Variance = Invariant
  )(implicit
    visited: Set[ScalaType]
  ): TypeParameterT[Tpe] =
    TypeParameter(
      tparam.psiTypeParameter,
      tparam.typeParameters.map(updateTypeParameter(_, substitutor, variance)),
      substitutor.recursiveUpdateImpl(tparam.lowerType, variance, isLazySubtype = true),
      substitutor.recursiveUpdateImpl(tparam.upperType, variance, isLazySubtype = true)
    )

  final def recursiveUpdate(tpe: Tpe, variance: Variance, update: Update[Tpe]): Tpe =
    update(tpe, variance) match {
      case ReplaceWith(res) => res
      case Stop             => tpe
      case ProcessSubtypes  => updateSubtypes(tpe, variance, ScSubstitutorT(update))(Set.empty)
    }
}

object SubtypeUpdater {
  implicit class TypeParameterUpdateExt[Tpe <: ScalaType](
    private val typeParameter: TypeParameterT[Tpe]
  ) extends AnyVal {
    def update(
      substitutor: ScSubstitutorT[Tpe]
    )(implicit
      updater: SubtypeUpdater[Tpe],
      visited: Set[ScalaType] = Set.empty
    ): TypeParameterT[Tpe] = updater.updateTypeParameter(typeParameter, substitutor)
  }

  implicit val scUpdater: SubtypeUpdater[ScType] = ScSubtypeUpdater.defaultUpdater
  implicit val dotUpdater: SubtypeUpdater[DotType] = DotSubtypeUpdater.defaultUpdater

  implicit def anyTpeUpdater(
    implicit
    scUpdater:  SubtypeUpdater[ScType],
    dotUpdater: SubtypeUpdater[DotType]
  ): SubtypeUpdater[ScalaType] = ScalaSubtypeUpdater(scUpdater, dotUpdater)
}
