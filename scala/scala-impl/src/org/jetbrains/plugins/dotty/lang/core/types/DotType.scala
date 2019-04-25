package org.jetbrains.plugins.dotty.lang.core
package types

import org.jetbrains.plugins.dotty.lang.core.types.api._
import org.jetbrains.plugins.dotty.lang.psi.types.{DottyTypePresentation, DottyTypeSystem}
import org.jetbrains.plugins.scala.lang.psi.types.{ScalaType, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.typeInference.{DotParameter, DotTypeParameter}

/**
  * Base supertypes for all classes, representing types in Dotty.
  * As a rule of thumb, the following naming conventions are used:
  * 1. Sc* prefix should be used for Scala 2 specific types/entities
  * 2. Dot* prefix should be used for Dotty specific types/entities
  * 3. Scala* prefix shouls be used for common supertypes and/or shared implementations
  *
  * See also: [[DotProxyType]] and [[DotGroundType]]
  */
sealed trait DotType extends ScalaType {
  override type Self = DotType
  override def typeSystem: DottyTypeSystem = ???
}

/**
  * Base super type for all proxy types in Dotty, where being a proxy type means
  * that such type will generally delegate most operations to some more general
  * "underlying" type.
  *
  * For example a singleton [[DotTermRef]] (`x.type`) will delegate to a (more general) type
  * of term `x` (e.g. `Int` or `String`).
  *
  * See also: [[DotGroundType]]
  */
sealed trait DotProxyType extends DotType {
  def underlying: DotType
}

sealed trait DotNamedType extends DotProxyType {
  def prefix: DotType
  def designator: Designator
}

/** The type of an empty prefix */
case object DotNoPrefix extends DotGroundType

/**
  * A type representing a reference to some type element (e.g. java.lang.String or p#T)
  * {{{
  * class P {
  *   type T
  * }
  * val p = new P
  * p#T // DotTypeRef(ScTypeAliasDeclaration(p#T))
  * }}}
  *
  * @param designator Type element, which is being referenced by this type. Must be class or type alias definition.
  */
final case class DotTypeRef private (
  override val prefix:     DotType,
  override val designator: Designator
) extends DotTypeRefApi with DotNamedType

/** Marker trait for types, populated with a single value (e.g. 1.type or x.type). */
sealed trait DotSingletonType extends DotProxyType

/**
  * A type, representing a reference to some term, e.g.
  *
  * {{{
  * object o {
  *   val x: Int = 123
  * }
  * o.x.type // DotTermRef(ScReferencePattern(o.x))
  * }}}
  *
  * @param designator Term element, which is being referenced by this type.
  */
final case class DotTermRef private (override val prefix: DotType, override val designator: Designator)
    extends DotTermRefApi
    with DotNamedType
    with DotSingletonType

/**
  * The type of `C.this.type`.
  *
  * @param thisElement An element representing template definition `C`.
  */
final case class DotThisType private (thisElement: ClassDenotation) extends DotThisTypeApi with DotSingletonType

/**
  * The type of a super reference, e.g. `C.super.type`
  *
  * @param thisRef  An element, representing template definition `C`
  * @param superRef The type of a value referenced by `super`
  */
final case class DotSuperType(
  thisRef:  ClassDenotation,
  superRef: DotType
) extends DotSingletonType {
  override def underlying: DotType = superRef
}

final case class DotConstantType(constant: Constant) extends DotConstantTypeApi with DotSingletonType

object DotConstantType {
  def unapply(tpe: DotConstantType): Constant = tpe.constant
}

/**
  * A refined type of form parent { refinement }, e.g. `C { type S <: T; val x: this.T }`
  *
  * @param parent The type being refined
  */
final case class DotRefinedType(
  parent:      DotType,
  refinement: Refinement
) extends DotRefinedTypeApi with DotProxyType

/** A type application of form [[tycon]][T_1, T_2, ... T_n]. */
final case class DotAppliedType(
  tycon: DotType,
  args:  List[DotType]
) extends DotAppliedTypeApi with DotProxyType

/** Internal type bounds representation `T` <: [[hi]] >: [[lo]] */
class DotTypeBounds(val lo: DotType, val hi: DotType) extends DotTypeBoundsApi with DotProxyType {
  final def get: DotTypeBounds = this
  final def isEmpty: Boolean   = false
  final def _1: DotType        = lo
  final def _2: DotType        = hi
}

object DotTypeBounds {
  def apply(lo: DotType, hi: DotType): DotTypeBounds = new DotTypeBounds(lo, hi)
  def unapply(arg: DotTypeBounds): DotTypeBounds = arg
}

/** A special case of [[DotTypeBounds]] when hi =:= lo, e.g. in declaration `type T = String` */
final case class DotAliasType(aliased: DotType) extends DotTypeBounds(aliased, aliased) with DotAliasTypeApi

final case class DotTypeVar(paramRef: DotTypeParameter) extends DotTypeVarApi with DotProxyType


/**
  * Base super type for all ground (as opposed to proxy) types in Dotty.
  *
  * See also: [[DotProxyType]]
  */
sealed trait DotGroundType extends DotType

/** Intersection type [[lhs]] & [[rhs]] */
final case class DotAndType private (
  lhs: DotType,
  rhs: DotType
) extends DotAndTypeApi with DotGroundType

/** Union type [[lhs]] | [[rhs]] */
final case class DotOrType private (
  lhs: DotType,
  rhs: DotType
) extends DotOrTypeApi with DotGroundType

/** Wildcard type `?` (possibly with bounds), used in type inference */
final case class DotWildcardType(bounds: Option[DotTypeBounds]) extends DotWildcardTypeApi with DotGroundType

sealed trait DotLambdaType

/** Type of a method def m(P1, ... Pn): [[resTpe]] */
final case class DotMethodType private (params: Seq[DotParameter], resTpe: DotType)
    extends DotMethodTypeApi
    with DotLambdaType
    with DotGroundType

/** Type of by-name parameter : =>[[resTpe]] or method w/o parameter lists */
final case class DotExprType private (resTpe: DotType)
  extends DotProxyType
    with DotExprTypeApi

/** Type of a polymorphic method def m[T1, ... Tn](P1, ... Pn): [[methodType]] */
final case class DotPolyType private (tparams: Seq[DotTypeParameter], methodType: DotMethodType)
    extends DotPolyTypeApi
    with DotLambdaType
    with DotGroundType

/** Type lambda of the form `[X1 <: U1 >: L1, ... Xn <: Un >: Ln] => T` */
final case class DotHKTypeLambda private (tparams: Seq[DotTypeParameter], resTpe: DotType)
  extends DotHKTypeLambdaApi
    with DotLambdaType
    with DotProxyType

/**
  * Type of the form [[scrutinee]] <: [[bound]] match { case_1, ... case_n }
  * where `case_i` is of shape [X1, .. Xn] => P => R
  */
final case class DotMatchType private (scrutinee: DotType, bound: DotType, cases: List[DotType])
    extends DotMatchTypeApi
    with DotProxyType
