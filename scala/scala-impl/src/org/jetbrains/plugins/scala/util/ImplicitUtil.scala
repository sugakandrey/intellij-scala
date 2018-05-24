package org.jetbrains.plugins.scala.util

import com.intellij.lang.{ASTNode, Language}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.impl.PsiElementBase
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi._
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.codeInspection.collections.MethodRepr
import org.jetbrains.plugins.scala.lang.psi.api.ImplicitParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructor, ScReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import org.jetbrains.plugins.scala.highlighter.usages.ScalaHighlightImplicitUsagesHandler.ImplicitTargetProvider._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.extensions._

import scala.annotation.tailrec

object ImplicitUtil {
  implicit class ImplicitTargetExt(val targetImplicit: PsiElement) extends AnyVal {
    private def isTarget(candidate: PsiElement): Boolean = candidate match {
      case `targetImplicit`                                                          => true
      case f: ScFunction if f.getSyntheticNavigationElement.contains(targetImplicit) => true
      case _                                                                         => false
    }

    private def matches(srr: ScalaResolveResult): Boolean =
      isTarget(srr.element) ||
        srr.implicitParameters.exists(matches) ||
        srr.implicitConversion.exists(matches)

    private def isImplicitConversionOrParameter(e: ScExpression): Boolean =
      e.implicitConversion().exists(matches) || isImplicitParameterOf(e)

    private def isImplicitParameterOf(e: ImplicitParametersOwner): Boolean =
      e.findImplicitParameters
        .getOrElse(Seq.empty)
        .exists(matches)

    def refOrImplicitRefIn(usage: PsiElement): Option[PsiReference] = usage match {
      case ref: ScReferenceElement if isTarget(ref.resolve())    => Option(ref)
      case e: ScExpression if isImplicitConversionOrParameter(e) => Option(ImplicitReference(e, targetImplicit))
      case st: ScSimpleTypeElement if isImplicitParameterOf(st)  => Option(ImplicitReference(st, targetImplicit))
      case _                                                     => None
    }
  }

  object ImplicitSearchTarget {
    def unapply(e: PsiElement): Option[PsiNamedElement] = e match {
      case named: ScNamedElement       => namedKind.target(named)
      case ref: ScReferenceElement     => refKind.target(ref)
      case contextBoundElement(tp, te) => contextBoundKind.target(tp, te)
      case _                           => None
    }
  }

  @tailrec
  private def range(usage: PsiElement): TextRange = {
    val simpleRange = usage.getTextRange

    def startingFrom(elem: PsiElement): TextRange = {
      val start = elem.getTextRange.getStartOffset
      TextRange.create(start, simpleRange.getEndOffset)
    }

    def forTypeElem(typeElem: ScSimpleTypeElement) = {
      def newTd =
        Option(PsiTreeUtil.getParentOfType(typeElem, classOf[ScNewTemplateDefinition]))
          .filter(_.constructor.flatMap(_.simpleTypeElement).contains(typeElem))

      def constructor =
        Option(PsiTreeUtil.getParentOfType(typeElem, classOf[ScConstructor]))
          .filter(_.simpleTypeElement.contains(typeElem))

      newTd
        .orElse(constructor)
        .getOrElse(typeElem)
        .getTextRange
    }

    usage match {
      case ScMethodCall(ScParenthesisedExpr(_), _)          => simpleRange
      case ScMethodCall(_: ScThisReference, _)              => simpleRange
      case MethodRepr(_: ScMethodCall, Some(base), None, _) => range(base)
      case MethodRepr(_, _, Some(ref), _)                   => startingFrom(ref.nameId)
      case simpleTypeElem: ScSimpleTypeElement              => forTypeElem(simpleTypeElem)
      case ref: ScReferenceElement                          => startingFrom(ref.nameId)
      case _                                                => simpleRange
    }
  }

  private[this] def relativeRangeInElement(usage: PsiElement): TextRange =
    range(usage).shiftLeft(usage.getTextRange.getStartOffset)

  final case class ImplicitReference(e: PsiElement, targetImplicit: PsiElement)
      extends PsiReferenceBase[PsiElement](e, relativeRangeInElement(e), false) {
    override def resolve(): PsiElement      = targetImplicit
    override def getVariants: Array[AnyRef] = Array.empty
  }

  final case class UnresolvedImplicitFakePsiElement(project: Project, file: PsiFile, lineOffset: Int)
      extends PsiElementBase {
    override def getContainingFile: PsiFile        = file
    override def isValid: Boolean                  = true
    override def getProject: Project               = project
    override def getTextRange: TextRange           = TextRange.EMPTY_RANGE.shiftRight(lineOffset)
    override def getTextOffset: Int                = lineOffset
    override def getText: String                   = ""
    override def getChildren: Array[PsiElement]    = PsiElement.EMPTY_ARRAY
    override def getTextLength: Int                = 0
    override def getStartOffsetInParent: Int       = 0
    override def textToCharArray(): Array[Char]    = Array.emptyCharArray
    override def getParent: PsiElement             = null
    override def getLanguage: Language             = ScalaLanguage.INSTANCE
    override def getNode: ASTNode                  = null
    override def findElementAt(i: Int): PsiElement = null
  }

  private[this] object UnresolvedImplicitFakePsiElement {
    def apply(targetImplicit: PsiElement, file: PsiFile, lineOffset: Int): UnresolvedImplicitFakePsiElement = {
      val project = inReadAction(targetImplicit.getProject)
      new UnresolvedImplicitFakePsiElement(project, file, lineOffset)
    }
  }

  final case class UnresolvedImplicitReference(targetImplicit: PsiElement, file: PsiFile, lineOffset: Int)
      extends PsiReferenceBase[PsiElement](
        UnresolvedImplicitFakePsiElement(targetImplicit, file, lineOffset),
        TextRange.EMPTY_RANGE,
        false
      ) {
    override def resolve(): PsiElement      = targetImplicit
    override def getVariants: Array[AnyRef] = Array.empty
  }

  object contextBoundElement {
    def unapply(e: PsiElement): Option[(ScTypeParam, ScTypeElement)] =
      if (e != null && e.getNode != null && e.getNode.getElementType == ScalaTokenTypes.tCOLON)
        (e.getParent, e.getNextSiblingNotWhitespaceComment) match {
          case (tp: ScTypeParam, te: ScTypeElement) => Some((tp, te))
          case _                                    => None
        } else None
  }
}
