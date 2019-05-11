package org.jetbrains.plugins.dotty.lang.core.symbols

import org.jetbrains.plugins.dotty.lang.core.types.{DotNoPrefix, DotTermRef, DotType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor

/** A symbol denoting a term (e.g. val/var decl) */
trait TermSymbol extends Symbol {
  protected def substitutor: ScSubstitutor

  /** [[DotTermRef]] instance, referencing this symbol */
  def termRef: DotTermRef = {
    val prefix = owner.fold(DotNoPrefix: DotType)(_.thisType)
    DotTermRef(prefix, this)
  }

  override def namedType: DotType = termRef
}
