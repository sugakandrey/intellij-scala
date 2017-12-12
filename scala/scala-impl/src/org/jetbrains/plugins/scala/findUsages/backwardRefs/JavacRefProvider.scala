package org.jetbrains.plugins.scala.findUsages.backwardRefs

import org.jetbrains.jps.javac.ast.api.JavacRef

trait JavacRefProvider[From] {
  def toJavacRef(from: From): Option[JavacRef]
}
