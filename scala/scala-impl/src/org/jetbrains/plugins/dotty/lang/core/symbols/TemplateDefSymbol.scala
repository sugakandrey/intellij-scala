package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeSymbol.TypeSymbolKind
import org.jetbrains.plugins.dotty.lang.core.types.{DotAppliedType, DotTemplateInfo, DotThisType, DotType}
import org.jetbrains.plugins.scala.lang.lexer.ScalaModifier
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameterT.DotTypeParameter
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet

/** Symbol denoting a template (i.e. class/object/trait) definition */
trait TemplateDefSymbol extends TypeSymbol {

  override def thisType: DotType = DotThisType(this)

  override def tpe: DotTemplateInfo

  def isSubClass(other: TemplateDefSymbol): Boolean = ???
}

object TemplateDefSymbol {


  final case class SyntheticTemplateSymbol(
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
    isThis:         Boolean = false
  ) extends TemplateDefSymbol {
    override def tpe: DotTemplateInfo = ??? // TODO

    override def appliedTypeRef: DotType =
      DotAppliedType(typeRef, typeParameters.map(TypeParamSymbol(_).typeRef))
  }

  def fromPsi(psi: ScTemplateDefinition): TemplateDefSymbol = ???
  def fromPsi(psi: PsiClass): TemplateDefSymbol             = ???
}
