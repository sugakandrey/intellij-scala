package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.types.ScalaType

trait StdTypes[Tpe <: ScalaType] {
  val Any: Tpe
  val AnyRef: Tpe
  val AnyVal: Tpe
  val Nothing: Tpe

  val Null: Tpe
  val Singleton: Tpe

  val Unit: Tpe
  val Boolean: Tpe
  val Char: Tpe
  val Byte: Tpe
  val Short: Tpe
  val Int: Tpe
  val Long: Tpe
  val Float: Tpe
  val Double: Tpe
}
