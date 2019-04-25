package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.openapi.util.Computable
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{Conformance, TypeSystem}

/**
  * @author adkozlov
  */
trait DottyConformance extends Conformance {
  typeSystem: TypeSystem[DotType] =>

  override protected def conformsComputable(key: Key, visited: Set[PsiClass]): Computable[ConstraintsResult] =
    () => ConstraintsResult.Left
}
