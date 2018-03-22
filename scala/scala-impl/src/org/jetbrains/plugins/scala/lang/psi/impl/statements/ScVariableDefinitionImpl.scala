package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScVariableStub
import org.jetbrains.plugins.scala.lang.psi.types.result._


/**
 * @author Alexander Podkhalyuzin
 */

class ScVariableDefinitionImpl private (stub: ScVariableStub, node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, VARIABLE_DEFINITION, node) with ScVariableDefinition {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScVariableStub) = this(stub, null)

  override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => super.accept(visitor)
      case _ => super.accept(visitor)
    }
  }

  def expr: Option[ScExpression] = byPsiOrStub(findChild(classOf[ScExpression]))(_.bodyExpression)

  override def toString: String = "ScVariableDefinition: " + ifReadAllowed(declaredNames.mkString(", "))("")

  def bindings: Seq[ScBindingPattern] = pList match {
    case null => Seq.empty
    case ScPatternList(Seq(pattern)) => pattern.bindings
    case ScPatternList(patterns) => patterns.flatMap(_.bindings)
  }

  def `type`(): TypeResult = typeElement match {
    case Some(te) => te.`type`()
    case None => expr.map(_.`type`())
            .getOrElse(Failure("Cannot infer type without an expression"))
  }

  def typeElement: Option[ScTypeElement] = byPsiOrStub(findChild(classOf[ScTypeElement]))(_.typeElement)

  def pList: ScPatternList = getStubOrPsiChild(PATTERN_LIST)

  def nameId: PsiElement = findChildByType[PsiElement](TokenSets.ID_SET)
}
