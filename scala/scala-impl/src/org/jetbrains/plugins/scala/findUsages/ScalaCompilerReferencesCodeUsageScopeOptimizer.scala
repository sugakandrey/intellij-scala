package org.jetbrains.plugins.scala.findUsages

import com.intellij.compiler.CompilerReferenceService
import com.intellij.psi.PsiElement
import com.intellij.psi.search.{ScopeOptimizer, SearchScope}

class ScalaCompilerReferencesCodeUsageScopeOptimizer extends ScopeOptimizer {
  override def getRestrictedUseScope(element: PsiElement): SearchScope =
    CompilerReferenceService.getInstance(element.getProject).getScopeWithoutCodeReferences(element)
}
