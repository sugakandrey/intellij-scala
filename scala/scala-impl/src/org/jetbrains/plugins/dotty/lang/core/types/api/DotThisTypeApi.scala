package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotThisType, DotType}

private[core] trait DotThisTypeApi { self: DotThisType =>
  def underlying: DotType = ???
}
