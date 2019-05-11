package org.jetbrains.plugins.dotty.lang.core.symbols

import org.jetbrains.plugins.dotty.lang.core.types.{DotNoPrefix, DotType, DotTypeRef}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.typeInference.DotTypeParameter

/** A symbol representing a type (e.g. type alias, type parameter or template definition) */
trait TypeSymbol extends Symbol {
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
}
