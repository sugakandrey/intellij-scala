package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeSymbol.TypeSymbolKind
import org.jetbrains.plugins.dotty.lang.core.types.{DotAppliedType, DotTemplateInfo, DotThisType, DotType}
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet
import org.jetbrains.plugins.scala.extensions._

/** Symbol denoting a template (i.e. class/object/trait) definition */
trait TemplateDefSymbol extends TypeSymbol {

  override def thisType: DotType = DotThisType(this)

  override def tpe: DotTemplateInfo

  override def toPsi(implicit scope: ElementScope): Option[PsiClass]

  def isSubClass(other: TemplateDefSymbol): Boolean = ???

  /** [[typeRef]] applied to this symbol type parameters */
  override def appliedTypeRef: DotType =
    DotAppliedType(typeRef, typeParameters.map(TypeParamSymbol(_).typeRef))
}

object TemplateDefSymbol {
  class TemplateDefSymbolImpl(
    override val packagePrefix:  Option[PackageDesignator],
    override val name:           String,
    val parents:                 Seq[DotType],
    override val owner:          Option[Symbol] = None,
    override val kind:           TypeSymbolKind = TypeSymbolKind.Class,
    override val typeParameters: Seq[DotTypeParameter] = Seq.empty,
    val modifiers:               EnumSet[ScalaModifier] = EnumSet.empty,
    val isPrivate:               Boolean = false,
    val isProtected:             Boolean = false,
    val isThis:                  Boolean = false,
    psiElement:                  () => Option[PsiClass] = () => None
  ) extends TemplateDefSymbol {
    override def toPsi(implicit scope: ElementScope): Option[PsiClass] = psiElement()
    override def tpe: DotTemplateInfo = ??? // TODO
  }

  def synthetic(
    packagePrefix:  Option[PackageDesignator],
    name:           String,
    parents:        Seq[DotType],
    owner:          Option[Symbol] = None,
    kind:           TypeSymbolKind = TypeSymbolKind.Class,
    typeParameters: Seq[DotTypeParameter] = Seq.empty,
    modifiers:      EnumSet[ScalaModifier] = EnumSet.empty,
    isPrivate:      Boolean = false,
    isProtected:    Boolean = false,
    isThis:         Boolean = false,
    psiElement:     () => Option[PsiClass] = () => None
  ): TemplateDefSymbol =
    new TemplateDefSymbolImpl(
      packagePrefix,
      name,
      parents,
      owner,
      kind,
      typeParameters,
      modifiers,
      isPrivate,
      isProtected,
      isThis,
      psiElement
    )

  def fromPsi(psi: ScTemplateDefinition): TemplateDefSymbol = ???
  def fromPsi(psi: PsiClass): TemplateDefSymbol = {
    val owner = psi.containingClass.toOption

    val packagePrefix =
      if (owner.isEmpty) psi.
  }
}
