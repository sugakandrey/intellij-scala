package org.jetbrains.plugins.dotty.lang.core

import java.util.concurrent.ConcurrentHashMap

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.dotty.lang.core.symbols.{Symbol, TemplateDefSymbol}
import org.jetbrains.plugins.dotty.lang.core.symbols.TypeSymbol.TypeSymbolKind
import org.jetbrains.plugins.dotty.lang.core.types.{DotAppliedType, DotHKTypeLambda, DotTemplateInfo, DotType}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.{ScalaModifier => Mod}
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.types.api.{StdTypes, Variance}
import org.jetbrains.plugins.scala.lang.typeInference.{DotTypeParameter, TypeParameter}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet

object DottyDefinitions {
  private[this] val maxImplementedFunctionArity: Int = 22

  /** Caches synthetic FunctionN symbols */
  private[this] val functionNSymbolCache = new ConcurrentHashMap[String, TemplateDefSymbol]()

  private def scalaStdClass(
    name:    String,
    flags:   EnumSet[Mod],
    parents: Seq[DotType] = Seq.empty
  ): TemplateDefSymbol =
    TemplateDefSymbol.synthetic(ScalaPkg.toOption, name, parents)

  val AnyNme: String          = "Any"
  val AnyRefName: String      = "AnyRef"
  val AnyKindNme: String      = "AnyKind"
  val AnyValNme: String       = "AnyVal"
  val NothingNme: String      = "Nothing"
  val UnitNme: String         = "Unit"
  val FunctionNPrefix: String = "Function"

  def ScalaPkg: PackageDesignator     = PackageDesignator("scala")
  def AnyClass: TemplateDefSymbol     = scalaStdClass(AnyNme, EnumSet(Mod.Abstract))
  def AnyRefClass: TemplateDefSymbol  = scalaStdClass(AnyRefName, EnumSet(Mod.Abstract))
  def AnyKindClass: TemplateDefSymbol = scalaStdClass(AnyKindNme, EnumSet(Mod.Abstract, Mod.Final))
  def AnyValClass: TemplateDefSymbol  = scalaStdClass(AnyValNme, EnumSet(Mod.Abstract), Seq(AnyTpe))
  def NothingClass: TemplateDefSymbol = scalaStdClass(NothingNme, EnumSet(Mod.Abstract, Mod.Final), Seq(AnyTpe))

  def TupleNClass(arity: Int)(implicit scope: ElementScope): TemplateDefSymbol = {
    ???
  }

  def FunctionNClass(arity: Int)(implicit scope: ElementScope): TemplateDefSymbol = {
    val className = s"Function$arity"
    functionNSymbolCache.computeIfAbsent(className, _ => FunctionNSymbol(arity))
  }

  lazy val AnyTpe: DotType     = AnyClass.typeRef
  lazy val AnyRefTpe: DotType  = AnyRefClass.typeRef
  lazy val AnyValTpe: DotType  = AnyValClass.typeRef
  lazy val NothingTpe: DotType = NothingClass.typeRef
  lazy val UnitTpe: DotType    = NothingClass.typeRef

  def FunctionTypeRef(arity: Int)(implicit scope: ElementScope): DotType =
    FunctionNClass(arity).typeRef

  def TupleNTypeRef(arity: Int)(implicit scope: ElementScope): DotType =
    TupleNClass(arity).typeRef

  /** Higher-kinded `Any` type, used in type bounds.
   * If [[tparams]] is empty returns simple kinded `Any` type instead.
   */
  def HKAny(tparams: Seq[DotTypeParameter])(implicit std: StdTypes[DotType]): DotType =
    DotHKTypeLambda(tparams, std.Any)

  def TupleType(paramTpes: Seq[DotType])(implicit scope: ElementScope): DotType = {
    val tupleNType = TupleNTypeRef(paramTpes.size)
    DotAppliedType(tupleNType, paramTpes)
  }

  def FunctionType(
    paramTpes: Seq[DotType],
    resTpe:    DotType
  )(implicit
    scope: ElementScope
  ): DotType = {
    val functionNType = FunctionTypeRef(paramTpes.size)
    DotAppliedType(functionNType, paramTpes.toList)
  }

  private final case class FunctionNSymbol(arity: Int)(implicit ctx: ProjectContext /* TODO: remove */) extends TemplateDefSymbol {
    override def name: String                             = s"Function$arity"
    override def owner: Option[Symbol]                    = None
    override def packagePrefix: Option[PackageDesignator] = ScalaPkg.toOption
    override protected def kind: TypeSymbolKind           = TypeSymbolKind.Trait

    override def typeParameters: Seq[DotTypeParameter] = {
      def typeParam(name: String, variance: Variance): DotTypeParameter =
        TypeParameter.light(name, Seq.empty, NothingTpe, AnyTpe, variance)

      val resultTypeParam = typeParam("R", Variance.Covariant)
      val argTypeParams   = (1 until arity).map(idx => typeParam("T" + idx, Variance.Contravariant))
      argTypeParams :+ resultTypeParam
    }

    override def toPsi(implicit scope: ElementScope): Option[PsiClass] =
      if (arity <= maxImplementedFunctionArity) scope.getCachedClass(name)
      else                                      None

    override def tpe: DotTemplateInfo = ???
  }
}
