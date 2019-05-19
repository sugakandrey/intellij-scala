package org.jetbrains.plugins.dotty.lang.core.types
package api

trait DotSuperTypeApi { this: DotSuperType =>
  def underlying: DotType = superTpe

  override def superType: DotType = ???
}
