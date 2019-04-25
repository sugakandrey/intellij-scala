package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.types.{ScalaType, TypePresentationContext}

/**
 * @author adkozlov
 */
trait TypePresentation[Tpe <: ScalaType] {
  def presentableText(
    tpe:        Tpe,
    withPrefix: Boolean = false
  )(implicit
    context: TypePresentationContext
  ): String

  def canonicalText(tpe: Tpe): String
  def urlText(tpe:       Tpe): String
}
