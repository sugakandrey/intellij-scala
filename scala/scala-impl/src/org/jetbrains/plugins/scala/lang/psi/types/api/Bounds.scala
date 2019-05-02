package org.jetbrains.plugins.scala.lang.psi.types.api

import org.jetbrains.plugins.scala.lang.psi.types.ScalaType

/**
 * @author adkozlov
 */
trait Bounds[Tpe <: ScalaType] {
  def lub(types: Seq[Tpe], checkWeak: Boolean): Tpe
  def glb(types: Seq[Tpe], checkWeak: Boolean): Tpe

  def glb(first: Tpe, second: Tpe, checkWeak: Boolean = false): Tpe
  def lub(first: Tpe, second: Tpe, checkWeak: Boolean = true): Tpe
}
