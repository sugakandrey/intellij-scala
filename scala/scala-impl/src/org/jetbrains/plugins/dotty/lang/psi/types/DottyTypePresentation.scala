package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.psi.PsiNamedElement
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.{TypeSystem, TypePresentation}

/**
  * @author adkozlov
  */
trait DottyTypePresentation extends TypePresentation {
  typeSystem: TypeSystem =>

  override protected def typeText(`type`: ScType,
                                  nameFun: PsiNamedElement => String,
                                  nameWithPointFun: PsiNamedElement => String)
                                 (implicit context: TypePresentationContext): String = "DottyType"
}
