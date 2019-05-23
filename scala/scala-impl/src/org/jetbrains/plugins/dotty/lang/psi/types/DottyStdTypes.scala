package org.jetbrains.plugins.dotty.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.DottyDefinitions
import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.{StdTypes, TypeSystem}

trait DottyStdTypes extends StdTypes[DotType] { this: TypeSystem[DotType] =>
  override lazy val Any: DotType     = DottyDefinitions.AnyTpe
  override lazy val AnyRef: DotType  = DottyDefinitions.AnyRefTpe
  override lazy val AnyVal: DotType  = DottyDefinitions.AnyValTpe
  override lazy val Nothing: DotType = DottyDefinitions.NothingTpe

  override lazy val Null: DotType      = ???
  override lazy val Singleton: DotType = ???

  override lazy val Unit: DotType    = ???
  override lazy val Boolean: DotType = ???
  override lazy val Char: DotType    = ???
  override lazy val Byte: DotType    = ???
  override lazy val Short: DotType   = ???
  override lazy val Int: DotType     = ???
  override lazy val Long: DotType    = ???
  override lazy val Float: DotType   = ???
  override lazy val Double: DotType  = ???
}
