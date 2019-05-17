package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.Equivalence

/**
 * @author adkozlov
 */
trait DottyEquivalence extends Equivalence[DotType] {
  typeSystem: DottyTypeSystem =>

  override def equiv(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem = emptyConstraints
  ): DotConstraintsResult = equiv(lhs, rhs, frozen = false, constraints = constraints)

  /** In dotty equivalence (`=:=`) is exactly mutual conformance.\
    * (This is not the case in Scala2).
    */
  def equiv(
    lhs:         DotType,
    rhs:         DotType,
    frozen:      Boolean,
    constraints: DotConstraintSystem,
  ): DotConstraintsResult = for {
    lhsConforms <- conforms(lhs, rhs, frozen, constraints)
    rhsConforms <- conforms(lhs, rhs, frozen, lhsConforms)
  } yield rhsConforms
}
