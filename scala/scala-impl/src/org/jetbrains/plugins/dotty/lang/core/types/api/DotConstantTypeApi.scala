package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotConstantTypeApi { self: DotConstantType =>
  def underlying: DotType = constant.tpe
}
