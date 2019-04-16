package org.jetbrains.plugins.dotty.lang.core

import org.jetbrains.plugins.dotty.lang.core.types.DotType

trait Designator {
  def tpe: DotType
  def name: String
}
