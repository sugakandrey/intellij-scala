package org.jetbrains.plugins.scala
package lang
package psi
package api
package expr

import _root_.org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScPathElement, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition

/**
  * @author Alexander Podkhalyuzin
  *         Date: 14.03.2008
  */
trait ScSuperReference extends ScExpression with ScPathElement {
  /**
    * @return is reference in decompiled file from Self type class
    */
  def isHardCoded: Boolean

  /** Type of `S` for `F.super[S]` */
  def staticSuper: Option[ScType]

  //name of super type as written in code
  def staticSuperName: String

  //for A.super or simply super
  def drvTemplate: Option[ScTemplateDefinition]

  def reference: Option[ScStableCodeReference] = findChild(classOf[ScStableCodeReference])

  override protected def acceptScala(visitor: ScalaElementVisitor): Unit = {
    visitor.visitSuperReference(this)
  }
}