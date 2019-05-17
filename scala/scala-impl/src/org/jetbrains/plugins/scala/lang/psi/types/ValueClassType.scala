package org.jetbrains.plugins.scala
package lang.psi.types

import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.extensions.{PsiClassExt, PsiElementExt, PsiParameterExt}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPrimaryConstructor
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.types.api.ExtractClass

/**
 * Nikolay.Tropin
 * 2014-10-02
 */
object ValueClassType {
  def unapply(tp: ScType): Option[ScType] = {
    tp match {
      case _: ValType => None
      case ExtractClass(cl: ScClass) if isValueClass(cl) =>
        cl.constructors match {
          case Seq(pc: ScPrimaryConstructor) =>
            pc.parameters.headOption.map(_.paramType())
          case _ => None
        }
      case _ => None
    }
  }

  def isValueType(tp: ScType): Boolean = unapply(tp).isDefined

  def isValueClass(cl: PsiClass): Boolean = cl match {
    case scClass: ScClass =>
      scClass.parameters match {
        case Seq(p) if isValOrCompiled(p) => extendsAnyVal(cl)
        case _ => false
      }
    case _ => false
  }

  def extendsAnyVal(cl: PsiClass): Boolean = cl.getSupers.map(_.qualifiedName).contains("scala.AnyVal")

  private def isValOrCompiled(p: ScClassParameter) = {
    if (p.isVal || p.isCaseClassVal) true
    else p.containingScalaFile.exists(_.isCompiled)
  }
}
