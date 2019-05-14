package org.jetbrains.plugins.dotty.lang.core.types

import org.jetbrains.plugins.dotty.lang.core.PackageDesignator
import org.jetbrains.plugins.dotty.lang.core.symbols.TemplateDefSymbol
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.{ScalaModifier => Mod}
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter
import org.jetbrains.plugins.scala.util.EnumSet
import org.jetbrains.plugins.scala.util.EnumSet.EnumSet

object DottyDefinitions {
  val maxImplementedFunctionArity: Int = 22

  private def scalaStdClass(
    name:    String,
    flags:   EnumSet[Mod],
    parents: Seq[DotType] = Seq.empty
  ): TemplateDefSymbol =
    TemplateDefSymbol.SyntheticTemplateSymbol(ScalaPkg.toOption, None, name, parents)

  val AnyNme: String     = "Any"
  val AnyKindNme: String = "AnyKind"
  val AnyValNme: String  = "AnyVal"
  val NothingNme: String = "Nothing"
  val UnitNme: String    = "Unit"

  def ScalaPkg: PackageDesignator     = PackageDesignator("scala")
  def AnyClass: TemplateDefSymbol     = scalaStdClass(AnyNme, EnumSet(Mod.Abstract))
  def AnyKindClass: TemplateDefSymbol = scalaStdClass(AnyKindNme, EnumSet(Mod.Abstract, Mod.Final))
  def AnyValClass: TemplateDefSymbol  = scalaStdClass(AnyValNme, EnumSet(Mod.Abstract), Seq(AnyTpe))
  def NothingClass: TemplateDefSymbol = scalaStdClass(NothingNme, EnumSet(Mod.Abstract, Mod.Final), Seq(AnyTpe))

  def FunctionNClass(arity: Int): TemplateDefSymbol = ???

  lazy val AnyTpe: DotType     = AnyClass.typeRef
  lazy val NothingTpe: DotType = NothingClass.typeRef
  lazy val UnitTpe: DotType    = NothingClass.typeRef
  lazy val AnyValTpe: DotType  = AnyValClass.typeRef

  def FunctionType(arity: Int): DotType =
    FunctionNClass(arity).typeRef

  /** Higher-kinded `Any` type, used in type bounds.
   * If [[tparams]] is empty returns simple kinded `Any` type instead.
   */
  def HKAny(tparams: Seq[DotTypeParameter])(implicit std: StdTypes[DotType]): DotType =
    DotHKTypeLambda(tparams, std.Any)

  def FunctionType(paramTpes: Seq[DotType], resTpe: DotType): DotType = {
    val functionNType = FunctionType(paramTpes.size)
    DotAppliedType(functionNType, paramTpes.toList)
  }
}
