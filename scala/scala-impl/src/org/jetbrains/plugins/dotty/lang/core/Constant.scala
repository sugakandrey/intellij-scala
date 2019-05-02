package org.jetbrains.plugins.dotty.lang.core

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.typeInference.ConstantTag

final case class Constant(value: Any, tag: ConstantTag) {
  def tpe: DotType = ???

  def isEmpty: Boolean = false
  def get: Constant    = this
  def _1: Any          = value
}
