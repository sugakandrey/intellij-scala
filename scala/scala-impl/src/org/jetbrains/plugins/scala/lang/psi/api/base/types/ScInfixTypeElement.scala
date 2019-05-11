package org.jetbrains.plugins.scala
package lang
package psi
package api
package base
package types

trait ScInfixTypeElement
  extends ScInfixElement
  with ScDesugarizableToParametrizedTypeElement {

  def left: ScTypeElement = findChildByClassScala(classOf[ScTypeElement])

  def rightOption: Option[ScTypeElement] = findChildrenByClassScala(classOf[ScTypeElement]) match {
    case Array(_, right) => Some(right)
    case _               => None
  }

  override protected val typeName = "InfixType"

  type Kind = ScTypeElement
  type Reference = ScStableCodeReference

  def operation: ScStableCodeReference = findChildByClassScala(classOf[ScStableCodeReference])

  override def desugarizedText = s"${operation.getText}[${left.getText}, ${rightOption.map(_.getText).getOrElse("Nothing")}]"
}

object ScInfixTypeElement {
  /** Extracts the left and right type elements of the given infix type. */
  def unapply(arg: ScInfixTypeElement): Option[(ScTypeElement, ScStableCodeReference, Option[ScTypeElement])] =
    Some((arg.left, arg.operation, arg.rightOption))
}