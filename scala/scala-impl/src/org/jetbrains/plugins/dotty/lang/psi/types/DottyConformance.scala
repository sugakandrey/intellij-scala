package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.openapi.util.Computable
import org.jetbrains.plugins.dotty.lang.core.symbols.{TemplateDefSymbol, TypeParamSymbol}
import org.jetbrains.plugins.dotty.lang.core.types._
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Conformance, TypeSystem}

trait DottyConformance extends Conformance[DotType] {
  typeSystem: TypeSystem[DotType] =>

  /**
   * Checks if the conformance relation (<:, "is subtype of"), as defined by SLS 3.5.2, holds.
   */
  override def conforms(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem
  ): DotConstraintsResult = conforms(lhs, rhs, constraints, frozen = false)

  def conforms(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem = emptyConstraints,
    frozen:      Boolean = false
  ): DotConstraintsResult =
    if (lhs == rhs || lhs.isNothing) constraints
    else if (/*todo: canBeSameOrInheritor */ true) {
      val key = CacheKey(lhs, rhs, frozen)
      val result = conformsPreventingRecursion(key, conformsComputable(key))
      result.combineWith(constraints)
    } else ConstraintsResult.Left

  private final def conformsInner(
    lhs:         DotType,
    rhs:         DotType,
    constraints: DotConstraintSystem,
    visited:     Set[TemplateDefSymbol],
    frozen:      Boolean = false,
  ): DotConstraintsResult = ???

  private def conformsComputable(key: CacheKey): Computable[DotConstraintsResult] =
    () => {
      val CacheKey(lhs, rhs, frozen) = key
      var constraints                = emptyConstraints

      def firstTry: DotConstraintsResult = rhs match {
        case tp2: DotNamedType =>
          def compareNamed(tp1: DotType, tp2: DotNamedType): DotConstraintsResult = tp2.underlying match {
            case DotAliasType(aliased) => conforms(tp1, aliased, constraints, frozen)
            case _ => tp1 match {
              case _ => secondTry()
            }
          }
          compareNamed(lhs, tp2)
        case DotThisType(sym2) =>
          lhs match {
            case DotThisType(sym1) =>
              sym1.self
            case
            case _ => secondTry()
          }
        case DotWildcardType(optBounds) => optBounds match {
          case Some(DotTypeBounds(_, hi)) => conforms(lhs, hi, constraints, frozen)
          case None                       => constraints
        }
        case DotAndType(tp21, tp22) =>
          for {
            conforms1 <- conforms(lhs, tp21, constraints, frozen)
            conforms2 <- conforms(lhs, tp22, constraints, frozen)
          } yield conforms1 + conforms2
        case DotConstantType(v2) => lhs match {
          case DotConstantType(v1) =>
            if (v1 == v2) constraints
            else          ConstraintsResult.Left
          case _ => secondTry()
        }
        case _ => ???
      }

      def secondTry(): DotConstraintsResult = lhs match {
        case DotTypeRef(_, TypeParamSymbol(tparam)) =>
          conforms(tparam.upperType, rhs, constraints, frozen = true)
        case DotWildcardType(optBounds) => optBounds match {
          case Some(DotTypeBounds(lo, _)) => conforms(lo, rhs, constraints, frozen)
          case None                       => constraints
        }

        case tp1: DotNamedType => ???
      }
    }
}
