package org.jetbrains.plugins.scala.highlighter.usages

import java.util

import com.intellij.codeInsight.highlighting.{HighlightUsagesHandler, HighlightUsagesHandlerBase}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.{PsiElement, PsiFile, PsiNamedElement, PsiReference, ReferenceRange}
import com.intellij.util.Consumer
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.highlighter.usages.ScalaHighlightImplicitUsagesHandler.ImplicitTargetProvider
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScMethodLike, ScReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.util.ImplicitUtil._

import scala.collection.JavaConverters._

class ScalaHighlightImplicitUsagesHandler[T](editor: Editor, file: PsiFile, data: T)
                                            (implicit kind: ImplicitTargetProvider[T])
    extends HighlightUsagesHandlerBase[PsiElement](editor, file) {

  override def getTargets: util.List[PsiElement] = (kind.target(data).toSeq: Seq[PsiElement]).asJava

  override def selectTargets(targets: util.List[PsiElement], selectionConsumer: Consumer[util.List[PsiElement]]): Unit =
    selectionConsumer.consume(targets)

  override def computeUsages(targets: util.List[PsiElement]): Unit = {
    import ScalaHighlightImplicitUsagesHandler._
    val usages = targets.asScala
      .flatMap(findUsages(file, _))
      .flatMap(ReferenceRange.getAbsoluteRanges(_).asScala)
    val targetIds = targets.asScala.flatMap(nameId)
    myReadUsages.addAll((targetIds ++ usages).asJava)
  }

  override def highlightReferences: Boolean = true

  override def highlightUsages(): Unit = {
    val targets = getTargets
    if (targets.isEmpty) {
      invokeDefaultHandler()
    } else {
      super.highlightUsages()
    }
  }

  //we want to avoid resolve in ScalaHighlightUsagesHandlerFactory, but also not to use ScalaHighlightImplicitUsagesHandler
  //for non-implicit elements
  private def invokeDefaultHandler(): Unit = {
    ScalaHighlightUsagesHandlerFactory.implicitHighlightingEnabled.set(false)
    try {
      HighlightUsagesHandler.invoke(editor.getProject, editor, file)
    } finally {
      ScalaHighlightUsagesHandlerFactory.implicitHighlightingEnabled.set(true)
    }
  }

  private def nameId(target: PsiElement): Option[TextRange] = target match {
    case named: ScNamedElement if named.getContainingFile == file => named.nameId.toOption.map(_.getTextRange)
    case _ => None
  }
}

object ScalaHighlightImplicitUsagesHandler {
  trait ImplicitTargetProvider[T] {
    def target(t: T): Option[PsiNamedElement]
  }

  object ImplicitTargetProvider {
    implicit val namedKind: ImplicitTargetProvider[ScNamedElement] = target(_)

    implicit val refKind: ImplicitTargetProvider[ScReferenceElement] = ref => ref.resolve match {
      case named: ScNamedElement => target(named)
      case _                     => None
    }

    implicit val contextBoundKind: ImplicitTargetProvider[(ScTypeParam, ScTypeElement)] = {
      case (typeParam, typeElem) => contextBoundImplicitTarget(typeParam, typeElem)
    }

    private def target(named: ScNamedElement): Option[PsiNamedElement] = named match {
        case _ if !named.isValid                             => None
        case c: ScClass                                      => c.getSyntheticImplicitMethod
        case n: ScNamedElement if ScalaPsiUtil.isImplicit(n) => Option(n)
        case _                                               => None
      }

    private def contextBoundImplicitTarget(typeParam: ScTypeParam, typeElem: ScTypeElement): Option[ScParameter] = {
      if (!typeElem.isValid) return None

      val typeParam = typeElem.getParent.asInstanceOf[ScTypeParam]
      val methodLike = typeParam.getOwner match {
        case fun: ScFunction => Some(fun)
        case c: ScClass => c.constructor
        case _ => None
      }
      def implicitParams(ml: ScMethodLike) =
        ml.effectiveParameterClauses
          .filter(_.isImplicit)
          .flatMap(_.effectiveParameters)

      val implicits = methodLike.map(implicitParams).getOrElse(Seq.empty)
      implicits.find { param =>
        (param.typeElement, typeElem.analog) match {
          case (Some(t1), Some(t2)) if t1.calcType == t2.calcType => true
          case _                                                  => false
        }
      }
    }
  }

  private def findUsages(file: PsiFile, target: PsiElement): Seq[PsiReference] = {
    val useScope = target.getUseScope
    if (!useScope.contains(file.getVirtualFile)) return Seq.empty

    def inUseScope(elem: PsiElement) = useScope match {
      case ls: LocalSearchScope => ls.containsRange(file, elem.getTextRange)
      case _ => true
    }

    file
      .depthFirst()
      .filter(inUseScope)
      .flatMap(target.refOrImplicitRefIn)
      .toSeq
  }
}
