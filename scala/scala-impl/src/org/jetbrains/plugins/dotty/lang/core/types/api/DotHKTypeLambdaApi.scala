package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotHKTypeLambdaApi { self: DotHKTypeLambda =>
  def underlying: DotType = resTpe
}
