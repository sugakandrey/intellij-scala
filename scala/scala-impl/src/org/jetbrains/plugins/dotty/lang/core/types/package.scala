package org.jetbrains.plugins.dotty.lang.core

package object types {
  implicit class DotTypeExt(private val tpe: DotType) extends AnyVal {
    def isAny: Boolean     = tpe == tpe.typeSystem.Any
    def isNothing: Boolean = tpe == tpe.typeSystem.Nothing

    def &(rhs: DotType): DotAndType = DotAndType(tpe, rhs)

    final def hiBound: DotType = tpe match {
      case DotTypeBounds(_, hi) => hi
      case _                    => tpe
    }

    /** Widen from [[DotExprType]] to it's result type */
    final def widenExprTpe: DotType = tpe match {
      case etpe: DotExprType => etpe.resTpe
      case _                 => tpe
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
  }
}
