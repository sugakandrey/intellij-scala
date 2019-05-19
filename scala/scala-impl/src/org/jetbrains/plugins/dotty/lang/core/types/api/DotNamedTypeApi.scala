package org.jetbrains.plugins.dotty.lang.core.types
package api

trait DotNamedTypeApi { this: DotNamedType =>
  def withPrefix(prefix: DotType): DotNamedType
}
