package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base

import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.annotator.ScTypeBoundsOwnerAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeBoundsOwner
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result._

trait ScTypeBoundsOwnerImpl extends ScTypeBoundsOwner with ScTypeBoundsOwnerAnnotator {

  def lowerBound: TypeResult = typeSystem.extractTypeBound(this, isLower = true).asSc

  def upperBound: TypeResult = typeSystem.extractTypeBound(this, isLower = false).asSc

  override def viewBound: Seq[ScType] = viewTypeElement.flatMap(_.`type`().toOption)

  override def contextBound: Seq[ScType] = contextBoundTypeElement.flatMap(_.`type`().toOption)

  override def upperTypeElement: Option[ScTypeElement] = {
    val tUpper = findLastChildByType[PsiElement](ScalaTokenTypes.tUPPER_BOUND)
    if (tUpper != null) {
      ScalaPsiUtil.getNextSiblingOfType(tUpper, classOf[ScTypeElement]) match {
        case null => None
        case te => Some(te)
      }
    } else None
  }

  override def lowerTypeElement: Option[ScTypeElement] = {
    val tLower = findLastChildByType[PsiElement](ScalaTokenTypes.tLOWER_BOUND)
    if (tLower != null) {
      ScalaPsiUtil.getNextSiblingOfType(tLower, classOf[ScTypeElement]) match {
        case null => None
        case te => Some(te)
      }
    } else None
  }


  override def viewTypeElement: Seq[ScTypeElement] = {
    for {
      v <- findChildrenByType(ScalaTokenTypes.tVIEW)
      e = ScalaPsiUtil.getNextSiblingOfType(v, classOf[ScTypeElement])
      t <- Option(e)
    } yield t
  }

  override def contextBoundTypeElement: Seq[ScTypeElement] = {
    for {
      v <- findChildrenByType(ScalaTokenTypes.tCOLON)
      t <- Option(ScalaPsiUtil.getNextSiblingOfType(v, classOf[ScTypeElement]))
    } yield t
  }

  override def removeImplicitBounds() {
    var node = getNode.getFirstChildNode
    while (node != null && !Set(ScalaTokenTypes.tCOLON, ScalaTokenTypes.tVIEW)(node.getElementType)) {
      node = node.getTreeNext
    }
    if (node == null) return
    node.getPsi.getPrevSibling match {
      case ws: PsiWhiteSpace => ws.delete()
      case _ =>
    }
    node.getTreeParent.removeRange(node, null)
  }
}