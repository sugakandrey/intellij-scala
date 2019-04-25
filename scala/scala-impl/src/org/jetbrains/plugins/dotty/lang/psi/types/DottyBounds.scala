package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Bounds, TypeSystem}

/**
  * @author adkozlov
  */
trait DottyBounds extends Bounds {
  typeSystem: TypeSystem[DotType] =>

  override def glb(first: ScType, second: ScType, checkWeak: Boolean): ScType = ???
  override def glb(types: Seq[ScType], checkWeak: Boolean): ScType = ???
  override def lub(first: ScType, second: ScType, checkWeak: Boolean): ScType = ???
  override def lub(types: Seq[ScType], checkWeak: Boolean): ScType = ???
}
