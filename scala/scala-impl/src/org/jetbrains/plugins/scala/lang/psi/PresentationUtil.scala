package org.jetbrains.plugins.scala.lang
package psi

import com.intellij.psi._
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocumentationProvider
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAccessModifier
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.SyntheticClasses
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScTypeUtil
import org.jetbrains.plugins.scala.lang.typeInference.ScalaParameter
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectContextOwner}

/**
 * User: Alexander Podkhalyuzin
 * Date: 12.08.2009
 */

object PresentationUtil {
  def presentationString(owner: ProjectContextOwner): String = {
    implicit val project = owner.projectContext
    presentationString(owner.asInstanceOf[Any])(project)
  }

  def presentationString(obj: Any)
                        (implicit project: ProjectContext): String = presentationString(obj, ScSubstitutor.empty)

  def presentationString(obj: Any, substitutor: ScSubstitutor)
                        (implicit project: ProjectContext): String = {
    val res: String = obj match {
      case clauses: ScParameters => clauses.clauses.map(presentationString(_, substitutor)).mkString("")
      case clause: ScParameterClause =>
        val buffer = new StringBuilder("")
        buffer.append("(")
        if (clause.isImplicit) buffer.append("implicit ")
        buffer.append(clause.parameters.map(presentationString(_, substitutor)).mkString(", "))
        buffer.append(")")
        buffer.toString()
      case param: ScParameter => ScalaDocumentationProvider.parseParameter(param)(presentationString(_, substitutor))
      case param: ScalaParameter =>
        val builder = new StringBuilder
        builder.append(param.name)
        builder.append(": " + presentationString(param.paramType, substitutor))
        if (param.isRepeated) builder.append("*")
        if (param.isDefault) builder.append(" = _")
        builder.toString()
      case tp: ScType => substitutor(tp).presentableText
      case tp: PsiEllipsisType =>
        presentationString(tp.getComponentType, substitutor) + "*"
      case tp: PsiType =>
        presentationString(tp.toScType(), substitutor)
      case tp: ScTypeParamClause =>
        tp.typeParameters.map(t => presentationString(t, substitutor)).mkString("[", ", ", "]")
      case param: ScTypeParam =>
        var paramText = param.name
        if (param.isContravariant) paramText = "-" + paramText
        else if (param.isCovariant) paramText = "+" + paramText
        val stdTypes = param.projectContext.stdTypes
        param.lowerBound foreach {
          case stdTypes.Nothing =>
          case tp: ScType => paramText = paramText + " >: " + presentationString(tp, substitutor)
        }
        param.upperBound foreach {
          case stdTypes.Any =>
          case tp: ScType => paramText = paramText + " <: " + presentationString(tp, substitutor)
        }
        param.viewBound foreach {
          (tp: ScType) => paramText = paramText + " <% " + presentationString(tp, substitutor)
        }
        param.contextBound foreach {
          (tp: ScType) => paramText = paramText + " : " + presentationString(ScTypeUtil.stripTypeArgs(substitutor(tp)), substitutor)
        }
        paramText
      case param: PsiTypeParameter =>
        var paramText = param.name
        //todo: possibly add supers and extends?
        paramText
      case params: PsiParameterList =>
        params.getParameters.map(presentationString(_, substitutor)).mkString("(", ", ", ")")
      case param: PsiParameter =>
        val buffer: StringBuilder = new StringBuilder("")
        val list = param.getModifierList
        if (list == null) return ""
        val lastSize = buffer.length
        for (a <- list.getAnnotations) {
          if (lastSize != buffer.length) buffer.append(" ")
          val element = a.getNameReferenceElement
          if (element != null) buffer.append("@").append(element.getText)
        }
        if (lastSize != buffer.length) buffer.append(" ")
        val name = param.name
        if (name != null) {
          buffer.append(name)
        }
        buffer.append(": ")
        buffer.append(presentationString(param.getType, substitutor)) //todo: create param type, java.lang.Object => Any
        buffer.toString()
      case fun: ScFunction =>
        val buffer: StringBuilder = new StringBuilder("")
        fun.getParent match {
          case _: ScTemplateBody if fun.containingClass != null =>
            val qual = fun.containingClass.qualifiedName
            if (qual != null) {
              buffer.append(qual).append(".")
            }
          case _ =>
        }
        buffer.append(fun.name)
        fun.typeParametersClause match {case Some(tpc) => buffer.append(presentationString(tpc)) case _ =>}
        buffer.append(presentationString(fun.paramClauses, substitutor)).append(": ")
        buffer.append(presentationString(fun.returnType.getOrAny, substitutor))
        buffer.toString()
      case elem: PsiElement => elem.getText
      case null => ""
      case _ => obj.toString
    }
    res.replace(SyntheticClasses.TypeParameter, "T")
  }

  def accessModifierText(am: ScAccessModifier): String = {
    val builder = new StringBuilder
    if (am.isPrivate) builder.append("private")
    else if (am.isProtected) builder.append("protected")
    else return ""
    if (am.isThis) {
      builder.append("[this]")
      return builder.toString()
    }
    am.idText match {
      case Some(id) => builder.append(s"[$id]")
      case _ =>
    }
    builder.toString()
  }
}
