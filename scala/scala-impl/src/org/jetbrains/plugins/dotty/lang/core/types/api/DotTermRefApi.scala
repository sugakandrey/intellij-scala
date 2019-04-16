package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.types.{DotTermRef, DotType}

private[core] trait DotTermRefApi { self: DotTermRef =>
  def underlying: DotType = designator.tpe
}
