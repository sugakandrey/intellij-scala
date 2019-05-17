package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.ide.util.EditSourceUtil
import com.intellij.lang.ASTNode
import com.intellij.navigation.ItemPresentation
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.IElementType
import com.intellij.psi.util.PsiTreeUtil.getNextSiblingOfType
import javax.swing.Icon
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes.{tLOWER_BOUND, tUPPER_BOUND}
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType.TYPE_DECLARATION
import org.jetbrains.plugins.scala.lang.psi.annotator.ScTypeBoundsOwnerAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createIdentifier
import org.jetbrains.plugins.scala.lang.psi.stubs.ScTypeAliasStub
import org.jetbrains.plugins.scala.lang.psi.types.api.{Any, Nothing}
import org.jetbrains.plugins.scala.lang.psi.types.result._

/**
* @author Alexander Podkhalyuzin
* Date: 22.02.2008
* Time: 9:54:54
*/

class ScTypeAliasDeclarationImpl private (stub: ScTypeAliasStub, node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, TYPE_DECLARATION, node) with ScTypeAliasDeclaration with ScTypeBoundsOwnerAnnotator {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScTypeAliasStub) = this(stub, null)

  override def getTextOffset: Int = nameId.getTextRange.getStartOffset

  override def navigate(requestFocus: Boolean) {
    val descriptor = EditSourceUtil.getDescriptor(nameId)
    if (descriptor != null) descriptor.navigate(requestFocus)
  }

  def nameId: PsiElement = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER) match {
    case null => createIdentifier(getGreenStub.getName).getPsi
    case n => n
  }

  override def toString: String = "ScTypeAliasDeclaration: " + ifReadAllowed(name)("")

  override def `type`(): TypeResult = Failure("ScTypeAliasDeclaration is not typeable in Scala 2.")

  override def upperTypeElement: Option[ScTypeElement] =
    byPsiOrStub(boundElement(tUPPER_BOUND))(_.upperBoundTypeElement)

  override def lowerTypeElement: Option[ScTypeElement] =
    byPsiOrStub(boundElement(tLOWER_BOUND))(_.lowerBoundTypeElement)

  private def boundElement(elementType: IElementType) = {
    val result = findLastChildByType[PsiElement](elementType) match {
      case null => null
      case element => getNextSiblingOfType(element, classOf[ScTypeElement])
    }
    Option(result)
  }

  override def getPresentation: ItemPresentation = {
    new ItemPresentation() {
      def getPresentableText: String = name
      def getTextAttributesKey: TextAttributesKey = null
      def getLocationString: String = "(" + ScTypeAliasDeclarationImpl.this.containingClass.qualifiedName + ")"
      override def getIcon(open: Boolean): Icon = ScTypeAliasDeclarationImpl.this.getIcon(0)
    }
  }

  override def getOriginalElement: PsiElement = super[ScTypeAliasDeclaration].getOriginalElement

  override protected def acceptScala(visitor: ScalaElementVisitor) {
    visitor.visitTypeAliasDeclaration(this)
  }
}