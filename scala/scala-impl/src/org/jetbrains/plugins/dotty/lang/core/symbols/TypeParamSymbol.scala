package org.jetbrains.plugins.dotty.lang.core.symbols

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.dotty.lang.core.types.{DotNoPrefix, DotTypeBounds, DotTypeRef}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference._
import org.jetbrains.plugins.scala.extensions._

trait TypeParamSymbol extends TypeSymbol {
  override protected def substitutor: ScSubstitutor = ???

  override def typeRef: DotTypeRef = DotTypeRef(DotNoPrefix, this)

  override def owner: Option[Symbol] = None

  /** An underlying type of the symbol */
  override def tpe: DotTypeBounds

  override def toPsi: Option[PsiTypeParameter] = typeParam.psiTypeParameter.toOption

  def typeParam: DotTypeParameter
}

object TypeParamSymbol {
  def apply(tparam: DotTypeParameter): TypeParamSymbol = ???

  def unapply(arg: TypeParamSymbol): Some[DotTypeParameter] = Some(arg.typeParam)
}
