package org.jetbrains.plugins.scala.findUsages.compilerReferences

import com.intellij.psi.{PsiElement, PsiReference}
import com.intellij.psi.search.RequestResultProcessor
import com.intellij.util.Processor
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScMember
import org.jetbrains.plugins.scala.extensions._

class ImplicitParameterUsageSearcher extends ImplicitUsageSearcher {
  override protected def requestResultProcessor(target: ScMember): RequestResultProcessor =
    new RequestResultProcessor() {
      override def processTextOccurrence(
        element: PsiElement,
        offsetInElement: Int,
        consumer: Processor[PsiReference]
      ): Boolean = inReadAction {
        
      }
    }
}
