package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.openapi.util.Computable
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Equivalence, TypeSystem}

/**
  * @author adkozlov
  */
trait DottyEquivalence extends Equivalence {
  typeSystem: TypeSystem =>

  override protected def equivComputable(key: Key): Computable[ConstraintsResult] =
    () => ConstraintsResult.Left
}
