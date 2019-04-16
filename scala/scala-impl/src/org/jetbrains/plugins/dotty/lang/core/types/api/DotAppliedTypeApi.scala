package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotAppliedType, DotType}

private[core] trait DotAppliedTypeApi { self: DotAppliedType =>
  def underlying: DotType = tycon
}
