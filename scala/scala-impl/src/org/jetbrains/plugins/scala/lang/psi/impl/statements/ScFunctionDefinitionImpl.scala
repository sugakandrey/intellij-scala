package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import com.intellij.psi._
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScFunctionStub
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScFunctionElementType

/**
  * @author Alexander Podkhalyuzin
  *         Date: 22.02.2008
  */

class ScFunctionDefinitionImpl(stub: ScFunctionStub[ScFunctionDefinition],
                               nodeType: ScFunctionElementType[ScFunctionDefinition],
                               node: ASTNode)
  extends ScFunctionImpl(stub, nodeType, node)
    with ScFunctionDefinition {

  override protected def shouldProcessParameters(lastParent: PsiElement): Boolean =
    super.shouldProcessParameters(lastParent) || body.contains(lastParent)

  override def toString: String = "ScFunctionDefinition: " + ifReadAllowed(name)("")

  def body: Option[ScExpression] = byPsiOrStub(findChild(classOf[ScExpression]))(_.bodyExpression)

  override def hasAssign: Boolean = byStubOrPsi(_.hasAssign)(assignment.isDefined)

  def assignment = Option(findChildByType[PsiElement](ScalaTokenTypes.tASSIGN))

  override def getBody: FakePsiCodeBlock = body match {
    case Some(b) => new FakePsiCodeBlock(b) // Needed so that LineBreakpoint.canAddLineBreakpoint allows line breakpoints on one-line method definitions
    case None => null
  }

  override protected def acceptScala(visitor: ScalaElementVisitor): Unit =
    visitor.visitFunctionDefinition(this)
}
