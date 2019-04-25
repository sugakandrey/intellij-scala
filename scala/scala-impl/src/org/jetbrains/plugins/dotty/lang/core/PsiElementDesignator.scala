package org.jetbrains.plugins.dotty.lang.core

import com.intellij.psi.PsiNamedElement
import org.jetbrains.plugins.dotty.lang.core.types.DotType

private[core] class PsiElementDesignator(e: PsiNamedElement) extends Designator {
  override def tpe: DotType = ???
  override def name: String = ???
}
