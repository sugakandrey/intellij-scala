package org.jetbrains.plugins.dotty.lang.core

import org.jetbrains.plugins.dotty.lang.core.symbols.{TemplateDefSymbol, TypeAliasSymbol, TypeSymbol}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystem
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter

package object types {
  implicit class DotTypeApi(private val tpe: DotType) extends AnyVal {
    @inline final def isAny: Boolean     = tpe == tpe.typeSystem.Any
    @inline final def isNothing: Boolean = tpe == tpe.typeSystem.Nothing

    @inline final def &(rhs: DotType): DotType = tpe.typeSystem.glb(tpe, rhs)
    @inline final def |(rhs: DotType): DotType = tpe.typeSystem.lub(tpe, rhs)

    final def <:<(other: DotType)(implicit ts: TypeSystem[DotType]): Boolean =
      ts.conforms(tpe, other, ts.emptyConstraints).isRight

    final def =:=(other: DotType)(implicit ts: TypeSystem[DotType]): Boolean =
      ts.equiv(tpe, other)

    final def isValueOrWildcard: Boolean = tpe match {
      case _: DotWildcardType => true
      case _: DotValueType    => true
      case _                  => false
    }

    final def hiBound: DotType = tpe match {
      case DotTypeBounds(_, hi) => hi
      case _                    => tpe
    }

    /** Widen from [[DotExprType]] to it's result type */
    final def widenExprTpe: DotType = tpe match {
      case etpe: DotExprType => etpe.resTpe
      case _                 => tpe
    }

    @annotation.tailrec
    final def dealias: DotType = tpe match {
      case tref @ DotTypeRef(_, sym) if sym.isAlias => tref.superType.dealias
      case _                                        => tpe
    }

    final def appliedTo(args: Seq[DotType]): DotType =
    if (args.isEmpty) tpe
    else {
      val dealiased = tpe.dealias
      dealiased match {
        case bounds: DotTypeBounds => bounds.updateRecursively {

        }
        case t if t.isNothing => t
        case _ => DotAppliedType(tpe, args)
      }
    }

    /**
     * Widen from [[DotSingletonType]] to it's underlying non-singleton type
     * applying one or more `.underlying` conversions.
     */
    @annotation.tailrec
    final def widenSingleton: DotType = tpe match {
      case singleton: DotSingletonType => singleton.underlying.widenSingleton
      case _                           => tpe
    }

    /**
     * Widen from singleton type to its underlying non-singleton
     * base type by applying one or more `underlying` dereferences,
     * Also go from => T to T.
     */
    @annotation.tailrec
    final def widen: DotType = tpe.widenSingleton match {
      case etpe: DotExprType => etpe.resTpe.widen
      case other             => other
    }

    final def widenUnion: DotType = tpe match {
      case orTpe: DotOrType        => ???
      case andTpe: DotAndType      => ???
      case refined: DotRefinedType => ???
      case _                       => tpe
    }

    final def typeParams: Seq[DotTypeParameter] = typeParams(tpe)

    /**
     * - For [[DotTemplateInfo]] type parameters of it's underlying template
     * - For [[DotTypeRef]] type parameters of the
     * - For [[DotRefinedType]] type parameters of it's parent type
     * @return
     */
    private final def typeParams(tp: DotType): Seq[DotTypeParameter] = tp match {
      case DotTypeRef(_, symbol)  => symbol.typeParameters
      case tInfo: DotTemplateInfo => ???
//      case wc: DotWildcardType                     => wc.bounds.fold(Nil)(_.typeParams)
      case lambda: DotHKTypeLambda                 => lambda.typeParams
      case _: DotSingletonType | _: DotRefinedType => Nil
      case proxy: DotProxyType                     => proxy.superType.typeParams
      case _                                       => Nil
    }

    final def applyIfParameterized(targs: Seq[DotType]): DotType =
      if (typeParams(tpe).nonEmpty) DotAppliedType(tpe, targs)
      else tpe

    /** The type symbol associated with [[tpe]] */
    @annotation.tailrec
    final def typeSymbol: Option[TypeSymbol] = tpe match {
      case tref: DotTypeRef      => tref.designator.toOption
      case info: DotTemplateInfo => info.tdef.toOption
      case _: DotSingletonType   => None
      case proxy: DotProxyType   => proxy.underlying.typeSymbol
      case _                     => None
    }

    final def tdefSymbol: Option[TemplateDefSymbol] = tpe match {
      case ctp: DotConstantType => ctp.underlying.tdefSymbol
      case tref @ DotTypeRef(_, sym) =>
        if (sym.isTemplate) sym.tdefSymbol.toOption
        else tref.superType.tdefSymbol
      case info: DotTemplateInfo => info.tdef.toOption
      case _: DotSingletonType   => None
      case proxy: DotProxyType   => proxy.underlying.tdefSymbol
      case DotAndType(lhs, rhs) =>
        for {
          lsym <- lhs.tdefSymbol
          rsym <- rhs.tdefSymbol
          res <- if (lsym.isSubClass(rsym)) lsym.toOption
                else if (rsym.isSubClass(lsym)) rsym.toOption
                else None
        } yield res
      case DotOrType(lhs, rhs) =>
        for {
          lsym <- lhs.tdefSymbol
          rsym <- rhs.tdefSymbol
          res <- if (lsym.isSubClass(rsym)) rsym.toOption
                else if (rsym.isSubClass(lsym)) lsym.toOption
                else None
        } yield res
      case _ => None
    }
  }
}
