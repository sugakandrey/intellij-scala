package org.jetbrains.plugins.dotty.lang.core.types.api

import org.jetbrains.plugins.dotty.lang.core.symbols.TypeParamSymbol
import org.jetbrains.plugins.dotty.lang.core.types.{DotType, DotTypeVar}

private[core] trait DotTypeVarApi { self: DotTypeVar =>
  val underlying: DotType = TypeParamSymbol(paramRef).typeRef
}
