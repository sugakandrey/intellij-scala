package org.jetbrains.plugins.scala.lang.psi.api

import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystem
import org.jetbrains.plugins.scala.lang.psi.types.{ScalaType, ScalaTypeSystem}
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectContextOwner}

trait ScalaPsiElement extends PsiElement with ProjectContextOwner with Annotatable {
  protected var context: PsiElement = null
  protected var child: PsiElement = null

  implicit def elementScope: ElementScope = ElementScope(this)

  implicit def projectContext: ProjectContext = this.getProject

  implicit def typeSystem: TypeSystem[_ <: ScalaType] = ScalaTypeSystem.instance // FIXME

  def isInCompiledFile: Boolean = getContainingFile match {
    case sf: ScalaFile => sf.isCompiled
    case _ => false
  }

  def setContext(element: PsiElement, child: PsiElement) {
    context = element
    this.child = child
  }

  def getSameElementInContext: PsiElement =
    child match {
      case null => this
      case _ => child
    }

  def getDeepSameElementInContext: PsiElement =
    child match {
      case null => this
      case _ if child == context => this
      case child: ScalaPsiElement => child.getDeepSameElementInContext
      case _ => child
    }

  def startOffsetInParent: Int =
    child match {
      case s: ScalaPsiElement => s.startOffsetInParent
      case _ => getStartOffsetInParent
    }

  protected def findChildByClassScala[T >: Null <: ScalaPsiElement](clazz: Class[T]): T

  protected def findChildrenByClassScala[T >: Null <: ScalaPsiElement](clazz: Class[T]): Array[T]

  protected def findChild[T >: Null <: ScalaPsiElement](clazz: Class[T]): Option[T] =
    Option(findChildByClassScala(clazz))

  def findLastChildByType[T <: PsiElement](t: IElementType): T = {
    var node = getNode.getLastChildNode
    while (node != null && node.getElementType != t) {
      node = node.getTreePrev
    }
    if (node == null) null.asInstanceOf[T]
    else node.getPsi.asInstanceOf[T]
  }

  def findFirstChildByType(t: IElementType): PsiElement = {
    var node = getNode.getFirstChildNode
    while (node != null && node.getElementType != t) {
      node = node.getTreeNext
    }
    if (node == null) null else node.getPsi
  }

  def findChildrenByType(t: IElementType): List[PsiElement] = {
    val buffer = new collection.mutable.ArrayBuffer[PsiElement]
    var node = getNode.getFirstChildNode
    while (node != null) {
      if (node.getElementType == t) buffer += node.getPsi
      node = node.getTreeNext
    }
    buffer.toList
  }

  def findLastChildByType(set: TokenSet): PsiElement = {
    var node = getNode.getLastChildNode
    while (node != null && !set.contains(node.getElementType)) {
      node = node.getTreePrev
    }
    if (node == null) null else node.getPsi
  }

  protected def findLastChild[T >: Null <: ScalaPsiElement](clazz: Class[T]): Option[T] = {
    var child = getLastChild
    while (child != null && !clazz.isInstance(child)) {
      child = child.getPrevSibling
    }
    if (child == null) None else Some(child.asInstanceOf[T])
  }

  abstract override def accept(visitor: PsiElementVisitor) {
    visitor match {
      case visitor: ScalaElementVisitor => acceptScala(visitor)
      case _ => super.accept(visitor)
    }
  }

  protected def acceptScala(visitor: ScalaElementVisitor): Unit = {
    visitor.visitScalaElement(this)
  }

  def acceptChildren(visitor: ScalaElementVisitor): Unit =
    getChildren.foreach {
      case element: ScalaPsiElement => element.accept(visitor)
      case _ =>
    }
}