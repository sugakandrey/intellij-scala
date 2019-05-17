package org.jetbrains.plugins.scala.codeInspection.typeChecking

import com.intellij.codeInspection.{ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.typeChecking.PatternMayNeverMatchInspection.{ScPatternExpectedAndPatternType, inspectionName}
import org.jetbrains.plugins.scala.codeInspection.{AbstractInspection, InspectionBundle}
import org.jetbrains.plugins.scala.lang.psi.annotator.ScPatternAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScPattern
import org.jetbrains.plugins.scala.lang.psi.types.ComparingUtil._
import org.jetbrains.plugins.scala.lang.psi.types.api.TypePresentationUtil
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, ScTypeExt}

/**
  * Author: Svyatoslav Ilinskiy
  * Date: 21.12.15.
  */
class PatternMayNeverMatchInspection extends AbstractInspection(inspectionName) {

  override def actionFor(implicit holder: ProblemsHolder): PartialFunction[PsiElement, Any] = {
    case pat@ScPatternExpectedAndPatternType(exTp, patType) =>
      if (!ScPatternAnnotator.matchesPattern(exTp, patType) && !patType.conforms(exTp) &&
        !isNeverSubType(exTp, patType)) {
        //need to check so inspection highlighting doesn't interfere with PatterAnnotator's
        val message = PatternMayNeverMatchInspection.message(exTp, patType)
        holder.registerProblem(pat, message, ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
      }
  }
}

object PatternMayNeverMatchInspection {
  val inspectionId = "PatternMayNeverMatch"
  val inspectionName = InspectionBundle.message("pattern.may.never.match")
  def message(_expected: ScType, _found: ScType): String = {
    val (expected, found) = TypePresentationUtil.different(_expected, _found)
    InspectionBundle.message("pattern.may.never.match", expected, found)
  }

  object ScPatternExpectedAndPatternType {
    def unapply(pat: ScPattern): Option[(ScType, ScType)] = {
      (pat.expectedType, ScPatternAnnotator.patternType(pat)) match {
        case (Some(expected), Some(pattern)) => Option((expected, pattern))
        case _ => None
      }
    }
  }
}
