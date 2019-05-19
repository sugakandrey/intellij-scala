package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotRefinedType, DotType}

private[core] trait DotRefinedTypeApi { self: DotRefinedType =>
  def underlying: DotType = parent

}
