package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.types.ScalaType

trait TypeSystemOwner {
  implicit def typeSystem: TypeSystem[_ <: ScalaType]
}
