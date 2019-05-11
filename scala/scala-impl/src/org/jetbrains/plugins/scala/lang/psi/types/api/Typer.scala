package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeBoundsOwner
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType
import org.jetbrains.plugins.scala.lang.psi.types.result.{TypeResultT, Typeable}

trait Typer[Tpe <: ScalaType] {
  def tpe(target: Typeable): TypeResultT[Tpe]

  /**
    * Given the type element of a type bound returns
    * actual type bound. It may differ from `typeElement.tpe` due to the
    * eta-expansion or simplification of parameterised
    * definition bounds to type lambdas
    * (e.g. `type F[X] <: Foo[X, X]` is in fact `type F <: [X] => Foo[X, X]` in dotty).
    */
  def extractTypeBound(
    owner:   ScTypeBoundsOwner,
    isLower: Boolean
  ): TypeResultT[Tpe]
}
