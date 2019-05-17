package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeSymbol.TypeSymbolKind
import org.jetbrains.plugins.dotty.lang.core.types.{
  DotAppliedType,
  DotTemplateInfo,
  DotThisType,
  DotType
}
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet

/** Symbol denoting a template (i.e. class/object/trait) definition */
trait TemplateDefSymbol extends TypeSymbol {

  override def thisType: DotType = DotThisType(this)

  override def tpe: DotTemplateInfo

  override def toPsi: Option[PsiClass]

  def isSubClass(other: TemplateDefSymbol): Boolean = ???
}

object TemplateDefSymbol {
  class TemplateDefSymbolImpl(
    override val packagePrefix:  Option[PackageDesignator],
    override val owner:          Option[Symbol],
    override val name:           String,
    val parents:                 Seq[DotType],
    override val kind:           TypeSymbolKind = TypeSymbolKind.Class,
    override val substitutor:    ScSubstitutor = ScSubstitutor.empty,
    override val typeParameters: Seq[DotTypeParameter] = Seq.empty,
    val modifiers:               EnumSet[ScalaModifier] = EnumSet.empty,
    val isPrivate:               Boolean = false,
    val isProtected:             Boolean = false,
    val isThis:                  Boolean = false,
    psiElement:                  () => Option[PsiClass] = () => None
  ) extends TemplateDefSymbol {
    override def toPsi: Option[PsiClass] = psiElement()

    override def appliedTypeRef: DotType =
      DotAppliedType(typeRef, typeParameters.map(TypeParamSymbol(_).typeRef))

    override def tpe: DotTemplateInfo = ??? // TODO
  }

  def synthetic(
    packagePrefix:  Option[PackageDesignator],
    owner:          Option[Symbol],
    name:           String,
    parents:        Seq[DotType],
    kind:           TypeSymbolKind = TypeSymbolKind.Class,
    substitutor:    ScSubstitutor = ScSubstitutor.empty,
    typeParameters: Seq[DotTypeParameter] = Seq.empty,
    modifiers:      EnumSet[ScalaModifier] = EnumSet.empty,
    isPrivate:      Boolean = false,
    isProtected:    Boolean = false,
    isThis:         Boolean = false,
    psiElement:     () => Option[PsiClass] = () => None
  ): TemplateDefSymbol =
    new TemplateDefSymbolImpl(
      packagePrefix,
      owner,
      name,
      parents,
      kind,
      substitutor,
      typeParameters,
      modifiers,
      isPrivate,
      isProtected,
      isThis,
      psiElement
    )

  def fromPsi(psi: ScTemplateDefinition): TemplateDefSymbol = ???
  def fromPsi(psi: PsiClass): TemplateDefSymbol = ???
}
