package org.jetbrains.plugins.scala.lang.psi
package types
package api

import org.jetbrains.plugins.scala.project.ProjectContextOwner

/**
 * @author adkozlov
 */
trait TypeSystem[Tpe <: ScalaType]
    extends ProjectContextOwner
    with Equivalence
    with Conformance
    with Bounds[Tpe]
    with PsiTypeBridge[Tpe]
    with TypePresentation[Tpe]
    with StdTypes[Tpe]
    with Typer[Tpe] {

  protected case class Key(left: ScType, right: ScType, flag: Boolean)

  val name: String

  override final def clearCache(): Unit = {
    super[Equivalence].clearCache()
    super[Conformance].clearCache()
  }
}

object TypeSystem {
  private[api] def combine(
    result:      ConstraintsResult
  )(constraints: ConstraintSystem
  ): ConstraintsResult = result match {
    case system: ConstraintSystem => if (constraints.isEmpty) system else system + constraints
    case _                        => ConstraintsResult.Left
  }
}
