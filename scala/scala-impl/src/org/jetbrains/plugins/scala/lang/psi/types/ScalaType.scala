package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystem

/**
 * Common supertype of both [[ScType]] and [[org.jetbrains.plugins.dotty.lang.core.types.DotType]]
 */
trait ScalaType { self =>
  type Self >: self.type <: ScalaType

  def typeSystem: TypeSystem[Self]
}
