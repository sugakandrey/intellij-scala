package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotMatchTypeApi { this: DotMatchType =>
  def underlying: DotType = bound
}
