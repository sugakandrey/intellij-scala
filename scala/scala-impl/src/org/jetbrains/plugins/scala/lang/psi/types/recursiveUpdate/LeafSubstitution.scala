package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith}
import org.jetbrains.plugins.scala.lang.psi.types.{LeafType, ScalaType}

/**
  * Nikolay.Tropin
  * 01-Feb-18
  */
private abstract class LeafSubstitution[Tpe <: ScalaType] extends SimpleUpdate[Tpe] {

  protected val subst: PartialFunction[LeafType, Tpe]

  def apply(tpe: Tpe): AfterUpdate[Tpe] = tpe match {
    //we shouldn't go deeper even if this substitution can't process `scType`
    //to allow application of several of them in the same pass
    case leaf: LeafType => ReplaceWith(subst.applyOrElse(leaf, Function.const(tpe)))
    case _              => ProcessSubtypes
  }
}

object LeafSubstitution {
  def apply[Tpe <: ScalaType](pf: PartialFunction[LeafType, Tpe]): SimpleUpdate[Tpe] =
    new LeafSubstitution[Tpe] {
      override protected val subst: PartialFunction[LeafType, Tpe] = pf
    }
}
