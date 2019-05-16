package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Equivalence, TypeSystem}

/**
  * @author adkozlov
  */
trait DottyEquivalence extends Equivalence[DotType] {
  typeSystem: TypeSystem[DotType] =>

  override def equiv(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem
  ): DotConstraintsResult = ???
}
