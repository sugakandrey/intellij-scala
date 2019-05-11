package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements
package params

import com.intellij.lang.ASTNode
import com.intellij.psi._
import com.intellij.psi.search.{LocalSearchScope, SearchScope}
import javax.swing.Icon
import org.jetbrains.plugins.dotty.lang.core.types.{DotHKTypeLambda, DotType}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes._
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScTypeBoundsOwnerImpl
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.PsiClassFake
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.JavaIdentifier
import org.jetbrains.plugins.scala.lang.psi.stubs.ScTypeParamStub
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScType, ScTypeExt, ScalaType}
import org.jetbrains.plugins.scala.macroAnnotations.{Cached, ModCount}

import scala.annotation.tailrec

/**
* @author Alexander Podkhalyuzin
* Date: 22.02.2008
*/

class ScTypeParamImpl private (stub: ScTypeParamStub, node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, ScalaElementType.TYPE_PARAM, node)
    with ScTypeBoundsOwnerImpl with ScTypeParam with PsiClassFake {

  def this(node: ASTNode) =  this(null, node)

  def this(stub: ScTypeParamStub) = this(stub, null)

  override lazy val typeParamId: Long = reusableId(this)

  override def toString: String = "TypeParameter: " + ifReadAllowed(name)("")

  def getContainingFileName: String = byStubOrPsi(_.containingFileName) {
    Option(getContainingFile).map(_.name).getOrElse("NoFile")
  }

  def getIndex: Int = 0
  def getOwner: PsiTypeParameterListOwner = getContext.getContext match {
    case c : PsiTypeParameterListOwner => c
    case _ => null
  }

  override def getContainingClass: ScTemplateDefinition = null

  @Cached(ModCount.anyScalaPsiModificationCount, this)
  def isCovariant: Boolean = byStubOrPsi(_.isCovariant) {
    Option(findChildByType[PsiElement](tIDENTIFIER))
      .exists(_.getText == "+")
  }

  @Cached(ModCount.anyScalaPsiModificationCount, this)
  def isContravariant: Boolean = byStubOrPsi(_.isContravariant) {
    Option(findChildByType[PsiElement](tIDENTIFIER))
      .exists(_.getText == "-")
  }

  def typeParameterText: String = byStubOrPsi(_.text)(getText)

  def owner: ScTypeParametersOwner = getContext.getContext.asInstanceOf[ScTypeParametersOwner]

  override def getUseScope: SearchScope = new LocalSearchScope(owner).intersectWith(super.getUseScope)

  def nameId: PsiElement = findLastChildByType(TokenSets.ID_SET)

  override def getNameIdentifier: PsiIdentifier = new JavaIdentifier(nameId)

  override def viewTypeElement: Seq[ScTypeElement] =
    byPsiOrStub(super.viewTypeElement)(_.viewBoundsTypeElements)

  override def contextBoundTypeElement: Seq[ScTypeElement] =
    byPsiOrStub(super.contextBoundTypeElement)(_.contextBoundsTypeElements)

  override def lowerTypeElement: Option[ScTypeElement] =
    byPsiOrStub(super.lowerTypeElement)(_.lowerBoundTypeElement)

  override def upperTypeElement: Option[ScTypeElement] =
    byPsiOrStub(super.upperTypeElement)(_.upperBoundTypeElement)

  override def getIcon(flags: Int): Icon = {
    Icons.TYPE_ALIAS
  }

  override def getSuperTypes: Array[PsiClassType] =
    // For Java
    upperBound.toOption.map {
      case ParameterizedType(des, _) if hasTypeParameters => des
      case t => t
    }.flatMap {
      _.toPsiType match {
        case x: PsiClassType => Some(x)
        case _ => None // TODO
      }
    }.toArray

  override def isHigherKindedTypeParameter: Boolean =
    this.parent.filter(_.isInstanceOf[ScTypeParamClause]).flatMap(_.parent).exists(_.isInstanceOf[ScTypeParam])
}
