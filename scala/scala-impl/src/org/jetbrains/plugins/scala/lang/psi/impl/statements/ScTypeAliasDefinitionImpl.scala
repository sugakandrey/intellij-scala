package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.ide.util.EditSourceUtil
import com.intellij.lang.ASTNode
import com.intellij.navigation.ItemPresentation
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi._
import javax.swing.Icon
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.annotator.ScTypeBoundsOwnerAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.ScalaElementVisitor
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createIdentifier
import org.jetbrains.plugins.scala.lang.psi.stubs.ScTypeAliasStub
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/**
* @author Alexander Podkhalyuzin
* Date: 22.02.2008
* Time: 9:55:13
*/
class ScTypeAliasDefinitionImpl private (stub: ScTypeAliasStub, node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, ScalaElementType.TYPE_DEFINITION, node) with ScTypeAliasDefinition with ScTypeBoundsOwnerAnnotator {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScTypeAliasStub) = this(stub, null)

  def nameId: PsiElement = findChildByType[PsiElement](ScalaTokenTypes.tIDENTIFIER) match {
    case null =>
      val name = getGreenStub.getName
      val id = createIdentifier(name)
      if (id == null) {
        assert(assertion = false, s"Id is null. Name: $name. Text: $getText. Parent text: ${getParent.getText}.")
      }
      id.getPsi
    case n => n
  }

  override def aliasedTypeElement: Option[ScTypeElement] =
    byPsiOrStub(Option(findChildByClassScala(classOf[ScTypeElement])))(_.typeElement)

  override def getTextOffset: Int = nameId.getTextRange.getStartOffset

  override def navigate(requestFocus: Boolean) {
    val descriptor =  EditSourceUtil.getDescriptor(nameId)
    if (descriptor != null) descriptor.navigate(requestFocus)
  }

  override def toString: String = "ScTypeAliasDefinition: " + ifReadAllowed(name)("")

  override def getPresentation: ItemPresentation = {
    new ItemPresentation() {
      def getPresentableText: String = name
      def getTextAttributesKey: TextAttributesKey = null
      def getLocationString: String = "(" + ScTypeAliasDefinitionImpl.this.containingClass.qualifiedName + ")"
      override def getIcon(open: Boolean): Icon = ScTypeAliasDefinitionImpl.this.getIcon(0)
    }
  }

  override def getOriginalElement: PsiElement = super[ScTypeAliasDefinition].getOriginalElement

  override protected def acceptScala(visitor: ScalaElementVisitor) {
    visitor.visitTypeAliasDefinition(this)
  }

  override def `type`(): TypeResult = aliasedType
}
