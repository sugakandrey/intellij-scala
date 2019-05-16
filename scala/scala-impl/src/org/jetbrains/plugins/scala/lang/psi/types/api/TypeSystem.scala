package org.jetbrains.plugins.scala.lang.psi
package types
package api

import org.jetbrains.plugins.scala.project.ProjectContextOwner

/**
 * @author adkozlov
 */
trait TypeSystem[Tpe <: ScalaType]
    extends ProjectContextOwner
    with Equivalence[Tpe]
    with Conformance[Tpe]
    with Bounds[Tpe]
    with PsiTypeBridge[Tpe]
    with TypePresentation[Tpe]
    with StdTypes[Tpe]
    with Typer[Tpe]
    with ConstraintHandling[Tpe] {

  protected implicit val ts: TypeSystem[Tpe] = this

  protected[types] final case class CacheKey(lhs: Tpe, rhs: Tpe, flag: Boolean)

  /** The symbolic identifier of this TypeSystem */
  val name: String

  override final def clearCache(): Unit = {
    super[Equivalence].clearCache()
    super[Conformance].clearCache()
  }
}
