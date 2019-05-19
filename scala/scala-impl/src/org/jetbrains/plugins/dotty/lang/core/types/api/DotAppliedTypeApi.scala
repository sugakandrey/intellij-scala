package org.jetbrains.plugins.dotty.lang.core.types
package api

private[core] trait DotAppliedTypeApi { this: DotAppliedType =>
  def underlying: DotType = tycon

  override def superType: DotType = tycon match {
    case _: DotHKTypeLambda                      => typeSystem.Any
    case DotTypeRef(_, symbol) if symbol.isClass => tycon
    case proxy: DotProxyType                     => proxy.superType.applyIfParameterized(typeArgs)
    case _                                       => typeSystem.Any

  }
}
