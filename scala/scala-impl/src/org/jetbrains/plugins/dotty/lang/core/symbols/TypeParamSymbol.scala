package org.jetbrains.plugins.dotty.lang.core.symbols

import org.jetbrains.plugins.dotty.lang.core.types.{DotNoPrefix, DotTypeBounds, DotTypeRef}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameterT.DotTypeParameter

trait TypeParamSymbol extends TypeSymbol {
  override protected def substitutor: ScSubstitutor = ???

  override def typeRef: DotTypeRef = DotTypeRef(DotNoPrefix, this)

  override def owner: Option[Symbol] = None

  /** An underlying type of the symbol */
  override def tpe: DotTypeBounds
}

object TypeParamSymbol {
  def apply(tparam: DotTypeParameter): TypeParamSymbol = ???
}
