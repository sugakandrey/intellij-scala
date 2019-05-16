package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.ConstraintSystem
import org.jetbrains.plugins.scala.lang.psi.types.api.ConstraintHandling

trait DottyConstraintHandling extends ConstraintHandling[DotType] {
  override def emptyConstraints: ConstraintSystem[DotType] = ???

  override def multiConstraintSystem(
    constraints: Set[ConstraintSystem[DotType]]
  ): ConstraintSystem[DotType] = ???
}
