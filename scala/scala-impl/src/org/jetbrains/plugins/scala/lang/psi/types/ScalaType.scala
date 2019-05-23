package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystem

import scala.language.implicitConversions

/**
 * Common supertype of both [[ScType]] and [[org.jetbrains.plugins.dotty.lang.core.types.DotType]]
 */
trait ScalaType { self =>
  type BaseTpe >: self.type <: ScalaType

  def typeSystem: TypeSystem[BaseTpe]
}

object ScalaType {
  implicit def recursiveUpdateExtension(tpe: ScalaType): recursiveUpdate.Extensions[ScalaType] =
    new recursiveUpdate.Extensions[ScalaType](tpe)
}
