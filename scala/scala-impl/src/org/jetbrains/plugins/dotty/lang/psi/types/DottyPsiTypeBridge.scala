package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.psi._
import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.{PsiTypeBridge, TypeSystem}


/**
  * @author adkozlov
  */
trait DottyPsiTypeBridge extends PsiTypeBridge[DotType] {
  typeSystem: TypeSystem[DotType] =>

  override def toScType(`type`: PsiType,
                        treatJavaObjectAsAny: Boolean)
                       (implicit visitedRawTypes: Set[PsiClass],
                        paramTopLevel: Boolean): DotType = ???

  override def toPsiType(`type`: DotType, noPrimitives: Boolean): PsiType = ???
}
