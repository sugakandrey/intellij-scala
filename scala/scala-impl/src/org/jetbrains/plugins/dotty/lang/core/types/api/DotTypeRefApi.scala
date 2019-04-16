package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotTypeRefApi { self: DotTypeRef =>
  def underlying: DotType = designator.tpe
}
