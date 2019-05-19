package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScalaType}

private[recursiveUpdate] final case class ScalaSubtypeUpdater(
  scUpdater:  SubtypeUpdater[ScType],
  dotUpdater: SubtypeUpdater[DotType]
) extends SubtypeUpdater[ScalaType] {

  override def withVariance: SubtypeUpdater[ScalaType] =
    copy(scUpdater = scUpdater.withVariance, dotUpdater = dotUpdater.withVariance)

  override def noVariance: SubtypeUpdater[ScalaType] =
    copy(scUpdater = scUpdater.noVariance, dotUpdater = dotUpdater.noVariance)

  override def traverser: SubtypeUpdater[ScalaType] =
    copy(scUpdater = scUpdater.traverser, dotUpdater = dotUpdater.traverser)

  override def updateSubtypes(
    tpe:         ScalaType,
    variance:    Variance,
    substitutor: ScSubstitutorT[ScalaType]
  )(implicit
    visited: Set[ScalaType]
  ): ScalaType = tpe match {
    case sc: ScType   => scUpdater.updateSubtypes(sc, variance, substitutor.narrow[ScType])
    case dot: DotType => dotUpdater.updateSubtypes(dot, variance, substitutor.narrow[DotType])
  }
}
