package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.types._

/**
 * Denotes a named, referencable entity in type related infrastructure
 * (e.g. (Type/Term)Refs, ThisTypes, SuperTypes etc.)
 * A generalisation of Psi elements, with thin, restricted interface.
 */
trait Symbol {

  /** Name of this symbol */
  def name: String

  /**
   * An owner of this symbol, or [[scala.None]] for locally defined and top-level symbols.
   */
  def owner: Option[Symbol]

  /** Enclosing package, or [[scala.None]] for non-top level symbols */
  def packagePrefix: Option[PackageDesignator]

  /** An underlying type of the symbol */
  def tpe: DotType

  /** [[DotThisType]] applied to this symbol for [[TemplateDefSymbol]],
    * [[DotNoPrefix]] for all other symbols
    */
  def thisType: DotType = DotNoPrefix

  /** [[DotTermRef]] or [[DotTypeRef]] pointing to this symbol */
  def namedType: DotType

  /** Finds corresponding [[PsiElement]] */
  def toPsi: Option[PsiElement]
}

object Symbol {
  def fromPsi(psi: PsiElement): Option[Symbol] = ???
}
