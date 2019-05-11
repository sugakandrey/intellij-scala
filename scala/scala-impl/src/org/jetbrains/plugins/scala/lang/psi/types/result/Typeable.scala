package org.jetbrains.plugins.scala.lang.psi
package types
package result

import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystemOwner

trait Typeable extends TypeSystemOwner {
  def `type`(): TypeResult

  /**
    * Returns the type of this entity.
    * Eventually will supersede [[`type`()]] after feature parity between DotType and ScType.
    */
  def tpe: ScalaTypeResult = typeSystem.tpe(this)

  def dotType: DotTypeResult = tpe.asDot
  def scType: TypeResult     = tpe.asSc
}

object Typeable {
  def unapply(typeable: Typeable): Option[ScType] = typeable match {
    case null => None
    case _    => typeable.`type`().toOption
  }
}
