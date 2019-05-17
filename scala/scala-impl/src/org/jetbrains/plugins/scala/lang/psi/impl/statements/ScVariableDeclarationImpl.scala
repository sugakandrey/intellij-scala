package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.annotator.ScVariableDeclarationAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.stubs.ScPropertyStub
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScPropertyElementType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/**
  * @author Alexander Podkhalyuzin
  */
final class ScVariableDeclarationImpl private[psi](stub: ScPropertyStub[ScVariableDeclaration],
                                                   nodeType: ScPropertyElementType[ScVariableDeclaration],
                                                   node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, nodeType, node) with ScVariableDeclaration with ScVariableDeclarationAnnotator {

  override def toString: String = "ScVariableDeclaration: " + ifReadAllowed(declaredNames.mkString(", "))("")

  def `type`(): TypeResult = typeElement.flatMapType

  def declaredElements: Seq[ScFieldId] = getIdList.fieldIds

  def typeElement: Option[ScTypeElement] = byPsiOrStub(findChild(classOf[ScTypeElement]))(_.typeElement)

  def getIdList: ScIdList = getStubOrPsiChild(ScalaElementType.IDENTIFIER_LIST)

  override protected def acceptScala(visitor: ScalaElementVisitor) {
    visitor.visitVariableDeclaration(this)
  }
}