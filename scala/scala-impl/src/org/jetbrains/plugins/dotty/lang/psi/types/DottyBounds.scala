package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.{DotAndType, DotAppliedType, DotExprType, DotOrType, DotRefinedType, DotType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{Bounds, TypeSystem}
import org.jetbrains.plugins.scala.extensions._

trait DottyBounds extends Bounds[DotType] {
  typeSystem: DottyTypeSystem =>

  override def lub(types: Seq[DotType], checkWeak: Boolean): DotType =
    types.foldLeft(Nothing)(lub(_, _))

  override def glb(types: Seq[DotType], checkWeak: Boolean): DotType =
    types.foldLeft(Any)(glb(_, _))

  override def glb(
    first:                                                 DotType,
    second:                                                DotType,
    @deprecated("Only makes sense for scala 2") checkWeak: Boolean = false
  ): DotType = ???

  override def lub(
    first:                                                 DotType,
    second:                                                DotType,
    @deprecated("Only makes sense for scala 2") checkWeak: Boolean = false
  ): DotType = ???

  /**
    * Merge [[lhs]] into [[rhs]] if it is a super type of some |-summand in [[rhs]]
    */
  private def mergeIfSuper(lhs: DotType, rhs: DotType, canConstrain: Boolean): Option[DotType] =
    if (conforms(rhs, lhs, frozen = !canConstrain).isRight) {
      if (conforms(lhs, rhs, frozen = !canConstrain).isRight) rhs.toOption
      else                                                    lhs.toOption } else
      rhs match {
        case DotOrType(rhs1, rhs2) =>
          val higher1 = mergeIfSuper(lhs, rhs1, canConstrain)
          higher1 match {
            case Some(h) if h eq rhs1 => rhs.toOption
            case Some(h)              => lub(h, rhs2).toOption
            case None =>
              val higher2 = mergeIfSuper(lhs, rhs2, canConstrain)
              higher2 match {
                case Some(h) if h eq rhs2 => rhs.toOption
                case Some(h)              => lub(rhs1, h).toOption
                case None                 => None
              }
          }
        case _ => None
      }

  /**
    * Merge [[lhs]] into [[rhs]] if it is a sub type of some &-summand in [[rhs]]
    */
  private def mergeIfSub(lhs: DotType, rhs: DotType): Option[DotType] =
    if (lhs <:< rhs) {
      if (rhs <:< lhs) lhs.toOption
      else             rhs.toOption
    } else
      rhs match {
        case DotAndType(rhs1, rhs2) =>
          val lower1 = mergeIfSub(lhs, rhs1)
          lower1 match {
            case Some(l) if l eq rhs1 => rhs.toOption
            case Some(l)              => glb(l, rhs2).toOption
            case None =>
              val lower2 = mergeIfSub(lhs, rhs2)
              lower2 match {
                case Some(l) if l eq rhs2 => rhs.toOption
                case Some(l)              => glb(rhs1, l).toOption
                case None                 => None
              }
          }
        case _ => None
      }

  private def distributeAnd(lhs: DotType, rhs: DotType): Option[DotType] = lhs match {
    case DotAppliedType(tycon, targs) => rhs match {
      case DotAppliedType(tycon2, targs2) if tycon =:= tycon2 =>
        val distributedArgs: Seq[DotType] = ???
          if () DotAppliedType
            else None
      case _ => None
    }
    case DotRefinedType()
    case DotExprType(lhsRes) => rhs match {
      case DotExprType(rhsRes) => glb(lhsRes, rhsRes).toOption
      case _ => None
    }
    case _ => None
  }

  private def distributeOr(lhs: DotType, rhs: DotType): Option[DotType] = lhs match {
    case DotExprType(lhsRes) =>
      rhs match {
        case DotExprType(rhsRes) => DotExprType(lub(lhsRes, rhsRes)).toOption
        case _                   => None
      }
    case _ => None
  }
}
