package org.jetbrains.plugins.dotty.lang.core.types
package api

trait DotExprTypeApi { this: DotExprType =>
  def underlying: DotType = resTpe
}
