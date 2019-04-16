package org.jetbrains.plugins.dotty.lang.core.types
package api

trait DotExprTypeApi { self: DotExprType =>
  def underlying: DotType = self.resTpe
}
