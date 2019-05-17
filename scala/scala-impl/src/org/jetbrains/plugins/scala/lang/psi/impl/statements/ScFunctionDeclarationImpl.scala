package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScFunctionStub
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScFunctionElementType

/**
  * @author Alexander Podkhalyuzin
  */
final class ScFunctionDeclarationImpl private[psi](stub: ScFunctionStub[ScFunctionDeclaration],
                                                   nodeType: ScFunctionElementType[ScFunctionDeclaration],
                                                   node: ASTNode)
  extends ScFunctionImpl(stub, nodeType, node)
    with ScFunctionDeclaration {

  override protected def acceptScala(visitor: ScalaElementVisitor): Unit =
    visitor.visitFunctionDeclaration(this)

  override def toString: String = "ScFunctionDeclaration: " + ifReadAllowed(name)("")
}

