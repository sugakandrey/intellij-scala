package org.jetbrains.plugins.dotty.lang.core

package object types {
  implicit class DotTypeExt(private val tpe: DotType) extends AnyVal {
    def isAny: Boolean = ???
    def isNothing: Boolean = ???
  }
}
