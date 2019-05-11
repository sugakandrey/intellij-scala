package org.jetbrains.plugins.dotty.lang.core
import com.intellij.psi.PsiPackage

final case class PackageDesignator private (qualifiedName: String) {}

object PackageDesignator {
  def apply(qname: String): PackageDesignator     = new PackageDesignator(qname)
  def fromPsi(psi: PsiPackage): PackageDesignator = PackageDesignator(psi.getQualifiedName)
}
