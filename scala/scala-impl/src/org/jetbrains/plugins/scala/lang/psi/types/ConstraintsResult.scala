package org.jetbrains.plugins.scala
package lang
package psi
package types

import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutorT

import scala.collection.immutable.LongMap

/** ConstraintsResult allows to represent failures in conformance and equivalence
 * without wrapping [[ConstraintSystem]] into a tuple
 */
sealed trait ConstraintsResult[+Tpe <: ScalaType]

object ConstraintsResult {

  implicit class ConstraintsResultExt[Tpe <: ScalaType](private val result: ConstraintsResult[Tpe])
      extends AnyVal {

    @inline final def map(
      f: ConstraintSystem[Tpe] => ConstraintSystem[Tpe]
    ): ConstraintsResult[Tpe] =
      result match {
        case Left                          => Left
        case system: ConstraintSystem[Tpe] => f(system)
      }

    @inline final def flatMap(
      f: ConstraintSystem[Tpe] => ConstraintsResult[Tpe]
    ): ConstraintsResult[Tpe] =
      result match {
        case Left                          => Left
        case system: ConstraintSystem[Tpe] => f(system)
      }

    @inline final def fold[B](ifEmpty: =>B)(f: ConstraintSystem[Tpe] => B): B =
      if (isLeft) ifEmpty else f(result.asInstanceOf[ConstraintSystem[Tpe]])

    @inline final def foreach[U](f: ConstraintSystem[Tpe] => U) {
      if (!isLeft) f(result.asInstanceOf[ConstraintSystem[Tpe]])
    }

    @inline final def withFilter(p: ConstraintSystem[Tpe] => Boolean): ConstraintsResult[Tpe] =
      result match {
        case Left                          => Left
        case system: ConstraintSystem[Tpe] => if (p(system)) system else Left
      }

    @inline final def isLeft: Boolean = result eq Left
    final def isRight: Boolean = !isLeft

    def constraints(implicit ts: TypeSystem[Tpe]): ConstraintSystem[Tpe] = result match {
      case Left => ts.emptyConstraints

      case system: ConstraintSystem[Tpe] => system
    }

    def combineWith(other: ConstraintSystem[Tpe]): ConstraintsResult[Tpe] = result match {
      case Left                          => Left
      case system: ConstraintSystem[Tpe] => if (other.isEmpty) system else system + other
    }
  }

  case object Left extends ConstraintsResult[Nothing]
}

/**
 * [[ConstraintSystem]] is used to accumulate information about type variables
 * during type inference.
 */
trait ConstraintSystem[Tpe <: ScalaType] extends ConstraintsResult[Tpe] {

  def isEmpty: Boolean

  def withTypeParamId(id: Long): ConstraintSystem[Tpe]

  def withLower(id: Long, lower: Tpe, variance: Variance = Contravariant): ConstraintSystem[Tpe]

  def withUpper(id: Long, upper: Tpe, variance: Variance = Covariant): ConstraintSystem[Tpe]

  def +(constraints: ConstraintSystem[Tpe]): ConstraintSystem[Tpe]

  def isApplicable(id: Long): Boolean

  def removeTypeParamIds(ids: Set[Long]): ConstraintSystem[Tpe]

  def substitutionBounds(canThrowSCE: Boolean): Option[SubstitutionBounds[Tpe]]

  def substitutor: Option[ScSubstitutorT[Tpe]] = substitutionBounds(true).map(_.substitutor)
}

/** A result of unifying constraints in a [[ConstraintSystem]].
  * Contains type variables substitutions [[tvMap]] and
  * type variables lower and upper bounds [[lowerMap]] & [[upperMap]]
  */
abstract class SubstitutionBounds[Tpe <: ScalaType](
  val tvMap:    LongMap[Tpe],
  val lowerMap: LongMap[Tpe],
  val upperMap: LongMap[Tpe]
) {
  def substitutor: ScSubstitutorT[Tpe]
}
