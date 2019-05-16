package org.jetbrains.plugins.scala.lang.psi.types

import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Computable
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType, ScThisType}

/**
 * User: Alexander Podkhalyuzin
 * Date: 28.04.2010
 */

trait ScalaEquivalence extends api.Equivalence[ScType] {
  typeSystem: api.TypeSystem[ScType] =>

  override def equiv(
    lhs:         ScType,
    rhs:         ScType,
    constraints: ScConstraintSystem
  ): ScConstraintsResult = equivInner(lhs, rhs, constraints)

  /**
    * @param falseUndef use false to consider [[UndefinedType]] type equivalent to any type
    */
  final def equivInner(
    left:        ScType,
    right:       ScType,
    constraints: ScConstraintSystem = emptyConstraints,
    falseUndef:  Boolean = true
  ): ScConstraintsResult = {
    ProgressManager.checkCanceled()

    if (left == right) constraints
    else if (left.canBeSameClass(right)) {
      val key    = CacheKey(left, right, falseUndef)
      val result = equivPreventingRecursion(key, equivComputable(key))
      result.combineWith(constraints)
    } else ConstraintsResult.Left
  }

  private def equivComputable(key: CacheKey): Computable[ScConstraintsResult] = new Computable[ScConstraintsResult] {
    import typeSystem.{emptyConstraints => empty}

    override def compute(): ScConstraintsResult = {
      val CacheKey(left, right, falseUndef) = key
      left match {
        case designator: ScDesignatorType => designator.getValType match {
          case Some(valType) => return equivInner(valType, right, falseUndef = falseUndef)
          case _ =>
        }
        case _ =>
      }

      right match {
        case designator: ScDesignatorType => designator.getValType match {
          case Some(valType) => return equivInner(left, valType, falseUndef = falseUndef)
          case _ =>
        }
        case _ =>
      }

      (left, right) match {
        case (UndefinedType(_, _), _) if right.isAliasType.isDefined =>
          val t = left.equivInner(right, empty, falseUndef)
          if (t.isRight) return t
        case (_, UndefinedType(_, _)) if left.isAliasType.isDefined =>
          val t = left.equivInner(right, empty, falseUndef)
          if (t.isRight) return t
        case (ParameterizedType(UndefinedType(_, _), _), _) if right.isAliasType.isDefined =>
          val t = left.equivInner(right, empty, falseUndef)
          if (t.isRight) return t
        case (_, ParameterizedType(UndefinedType(_, _), _)) if left.isAliasType.isDefined =>
          val t = right.equivInner(left, empty, falseUndef)
          if (t.isRight) return t
        case _ =>
      }

      right.isAliasType match {
        case Some(AliasType(_: ScTypeAliasDefinition, Right(right), _)) => return equivInner(left, right, falseUndef = falseUndef)
        case _ =>
      }

      left.isAliasType match {
        case Some(AliasType(_: ScTypeAliasDefinition, Right(left), _)) => return equivInner(left, right, falseUndef = falseUndef)
        case _ =>
      }

      (left, right) match {
        case (_, _: UndefinedType) => right.equivInner(left, empty, falseUndef)
        case (_: UndefinedType, _) => left.equivInner(right, empty, falseUndef)
        case (_, _: ScAbstractType) => right.equivInner(left, empty, falseUndef)
        case (_: ScAbstractType, _) => left.equivInner(right, empty, falseUndef)
        case (_, ParameterizedType(_: ScAbstractType, _)) => right.equivInner(left, empty, falseUndef)
        case (ParameterizedType(_: ScAbstractType, _), _) => left.equivInner(right, empty, falseUndef)
        case (_, t) if t.isAnyRef => right.equivInner(left, empty, falseUndef)
        case (_: StdType, _: ScProjectionType) => right.equivInner(left, empty, falseUndef)
        case (_: ScDesignatorType, _: ScThisType) => right.equivInner(left, empty, falseUndef)
        case (_: ScParameterizedType, _: JavaArrayType) => right.equivInner(left, empty, falseUndef)
        case (_, _: ScExistentialType) => right.equivInner(left, empty, falseUndef)
        case (_, _: ScProjectionType) => right.equivInner(left, empty, falseUndef)
        case (_, _: ScCompoundType) => right.equivInner(left, empty, falseUndef)
        case _ => left.equivInner(right, empty, falseUndef)
      }
    }
  }
}
