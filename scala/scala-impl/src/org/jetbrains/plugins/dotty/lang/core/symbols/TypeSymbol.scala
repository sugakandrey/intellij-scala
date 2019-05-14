package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeSymbol.TypeSymbolKind
import org.jetbrains.plugins.dotty.lang.core.types.{DotNoPrefix, DotType, DotTypeRef}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter

/** A symbol representing a type (e.g. type alias, type parameter or template definition) */
trait TypeSymbol extends Symbol {
  protected def kind: TypeSymbolKind

  protected def substitutor: ScSubstitutor

  def typeParameters: Seq[DotTypeParameter]

  /** [[DotTypeRef]] pointing to this type */
  def typeRef: DotTypeRef = {
    val prefix = owner.fold(DotNoPrefix: DotType)(_.thisType)
    DotTypeRef(prefix, this)
  }

  /** [[typeRef]] applied to this symbol type parameters */
  def appliedTypeRef: DotType

  override def namedType: DotType = typeRef

  def isClass: Boolean     = kind == TypeSymbolKind.Class
  def isObject: Boolean    = kind == TypeSymbolKind.Object
  def isTrait: Boolean     = kind == TypeSymbolKind.Trait
  def isTypeParam: Boolean = kind == TypeSymbolKind.TypeParam
  def isAlias: Boolean     = kind == TypeSymbolKind.TypeAlias
  def isTemplate: Boolean  = isClass || isTrait || isObject

  def tdefSymbol: TemplateDefSymbol = this.asInstanceOf[TemplateDefSymbol]
}

object TypeSymbol {
  sealed trait TypeSymbolKind
  object TypeSymbolKind {
    case object Class     extends TypeSymbolKind
    case object Trait     extends TypeSymbolKind
    case object Object    extends TypeSymbolKind
    case object TypeParam extends TypeSymbolKind
    case object TypeAlias extends TypeSymbolKind
  }

  def fromPsi(e: PsiElement): Option[TypeSymbol] = ???
}
