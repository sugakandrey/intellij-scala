package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutorT
import org.jetbrains.plugins.scala.lang.psi.types.{ConstraintSystem, ScalaType}

trait ConstraintHandling[Tpe <: ScalaType] {
  def emptyConstraints: ConstraintSystem[Tpe]

  def multiConstraintSystem(constraints: Set[ConstraintSystem[Tpe]]): ConstraintSystem[Tpe]

  final class ConstraintSystemUnapply {
    def unapply(system: ConstraintSystem[Tpe]): Option[ScSubstitutorT[Tpe]] =
      system.substitutionBounds(canThrowSCE = true).map(_.substitutor)
  }

  object Constraints {
    val withSubstitutor: ConstraintSystemUnapply = new ConstraintSystemUnapply
  }
}
