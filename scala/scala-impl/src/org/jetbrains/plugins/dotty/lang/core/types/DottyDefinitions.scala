package org.jetbrains.plugins.dotty.lang.core.types

import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.symbols.{TemplateDefSymbol, TypeSymbol}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.{ScalaModifier => Mod}
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet
import java.util.concurrent.ConcurrentHashMap

object DottyDefinitions {
  private[this] val maxImplementedFunctionArity: Int = 22

  /** Caches synthetic FunctionN symbols */
  private[this] val functionNSymbolCache = new ConcurrentHashMap[String, TemplateDefSymbol]()

  private def scalaStdClass(
    name:    String,
    flags:   EnumSet[Mod],
    parents: Seq[DotType] = Seq.empty
  ): TemplateDefSymbol =
    TemplateDefSymbol.SyntheticTemplateSymbol(ScalaPkg.toOption, None, name, parents)

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

  def TupleNClass(arity: Int)(implicit scope: ElementScope): TemplateDefSymbol = ???

  def FunctionNClass(arity: Int)(implicit scope: ElementScope): TemplateDefSymbol = {
    val className = s"Function$arity"
    functionNSymbolCache.computeIfAbsent(className, _ => {
      val tparams: Seq[DotTypeParameter] = ???
//      new TemplateDefSymbol.SyntheticTemplateSymbol(
//        ScalaPkg.toOption,
//        None,
//        className,
//        Seq(AnyRefTpe),
//        kind = TypeSymbol.TypeSymbolKind.Trait,
//        typeParameters = tparams
//      ) {
//
//      }
      ???
    })
  }

  lazy val AnyTpe: DotType     = AnyClass.typeRef
  lazy val AnyRefTpe: DotType  = AnyRefClass.typeRef
  lazy val AnyValTpe: DotType  = AnyValClass.typeRef
  lazy val NothingTpe: DotType = NothingClass.typeRef
  lazy val UnitTpe: DotType    = NothingClass.typeRef

  def FunctionType(arity: Int)(implicit scope: ElementScope): DotType =
    FunctionNClass(arity).typeRef

  /** Higher-kinded `Any` type, used in type bounds.
   * If [[tparams]] is empty returns simple kinded `Any` type instead.
   */
  def HKAny(tparams: Seq[DotTypeParameter])(implicit std: StdTypes[DotType]): DotType =
    DotHKTypeLambda(tparams, std.Any)

  def FunctionType(
    paramTpes: Seq[DotType],
    resTpe:    DotType
  )(implicit
    scope: ElementScope
  ): DotType = {
    val functionNType = FunctionType(paramTpes.size)
    DotAppliedType(functionNType, paramTpes.toList)
  }
}
