package org.jetbrains.plugins.dotty.lang.core.symbols

import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAlias

/** Symbol denoting type alias definiton */
trait TypeAliasSymbol extends TypeSymbol {
  override def toPsi: Option[ScTypeAlias]
}
