package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Conformance, TypeSystem}

/**
  * @author adkozlov
  */
trait DottyConformance extends Conformance[DotType] {
  typeSystem: TypeSystem[DotType] =>

  /**
   * Checks if the conformance relation (<:, "is subtype of"), as defined by SLS 3.5.2, holds.
   */
  override def conforms(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem
  ): DotConstraintsResult = ???
}
