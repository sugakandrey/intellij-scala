package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotType, DotTypeBounds}

private[core] trait DotTypeBoundsApi { self: DotTypeBounds =>
  def underlying: DotType = hi
}
