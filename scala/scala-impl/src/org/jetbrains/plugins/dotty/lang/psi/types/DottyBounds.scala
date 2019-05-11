package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Bounds, TypeSystem}

trait DottyBounds extends Bounds[DotType] {
  typeSystem: TypeSystem[DotType] =>

  override def lub(types: Seq[DotType], checkWeak: Boolean): DotType =
    types.foldLeft(Nothing)(lub(_, _))

  override def glb(types: Seq[DotType], checkWeak: Boolean): DotType =
    types.foldLeft(Any)(glb(_, _))

  override def glb(
    first:                 DotType,
    second:                DotType,
    @deprecated checkWeak: Boolean = false
  ): DotType = ???

  override def lub(
    first:                 DotType,
    second:                DotType,
    @deprecated checkWeak: Boolean = false
  ): DotType = ???
}
