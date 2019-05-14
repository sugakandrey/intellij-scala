package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotTypeRefApi { self: DotTypeRef =>
  def underlying: DotType = designator.tpe

  def withPrefix(prefix: DotType): DotNamedType =
    if (prefix eq this.prefix) this
    else                       DotTypeRef(prefix, designator)
}
