package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.lang.psi.types.api.{StdTypes, TypeSystem}

trait ScalaStdTypes extends StdTypes[ScType] { this: TypeSystem[ScType] =>
  private[this] lazy val types: ScStdTypes = ScStdTypes.instance

  override lazy val Any: ScType     = types.Any
  override lazy val AnyRef: ScType  = types.AnyRef
  override lazy val AnyVal: ScType  = types.AnyVal
  override lazy val Nothing: ScType = types.Nothing

  override lazy val Null: ScType      = types.Null
  override lazy val Singleton: ScType = types.Singleton

  override lazy val Unit: ScType    = types.Unit
  override lazy val Boolean: ScType = types.Boolean
  override lazy val Char: ScType    = types.Char
  override lazy val Byte: ScType    = types.Byte
  override lazy val Short: ScType   = types.Short
  override lazy val Int: ScType     = types.Int
  override lazy val Long: ScType    = types.Long
  override lazy val Float: ScType   = types.Float
  override lazy val Double: ScType  = types.Double
}
