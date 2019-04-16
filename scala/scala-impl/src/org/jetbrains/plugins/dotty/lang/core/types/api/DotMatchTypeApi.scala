package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotMatchTypeApi { self: DotMatchType =>
  def underlying: DotType = bound
}
