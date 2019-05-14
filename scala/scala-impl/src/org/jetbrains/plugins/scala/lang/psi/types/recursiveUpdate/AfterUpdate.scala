package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.lang.psi.types.ScalaType

sealed trait AfterUpdate[+Tpe <: ScalaType]

object AfterUpdate {
  case class ReplaceWith[Tpe <: ScalaType](scType: Tpe) extends AfterUpdate[Tpe]
  case object ProcessSubtypes extends AfterUpdate[Nothing]
  case object Stop            extends AfterUpdate[Nothing]
}
