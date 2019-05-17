package org.jetbrains.plugins.scala
package lang
package psi
package types

import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.NonValueType
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameter
import org.jetbrains.plugins.scala.project.ProjectContext



/**
 * This type works like undefined type, but you cannot use this type
 * to resolve generics. It's important if two local type
 * inferences work together.
 */
class ScAbstractType(val typeParameter: TypeParameter) extends ScType with NonValueType with LeafType {

  override implicit def projectContext: ProjectContext = typeParameter.psiTypeParameter

  def lower: ScType = typeParameter.lowerType

  def upper: ScType = typeParameter.upperType

  override def hashCode: Int = typeParameter.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: ScAbstractType => typeParameter == other.typeParameter
    case _ => false
  }

  override def equivInner(r: ScType, constraints: ConstraintSystem, falseUndef: Boolean): ConstraintsResult = {
    r match {
      case _ if falseUndef => ConstraintsResult.Left
      case _ =>
        val conformsUpper = r.conforms(upper, constraints)
        if (conformsUpper.isLeft) return ConstraintsResult.Left

        lower.conforms(r, conformsUpper.constraints)
    }
  }

  def inferValueType: TypeParameterType = TypeParameterType(typeParameter)

  def simplifyType: ScType = {
    if (upper.equiv(Any)) lower else if (lower.equiv(Nothing)) upper else lower
  }

  override def visitType(visitor: TypeVisitor): Unit = visitor match {
    case scalaVisitor: ScalaTypeVisitor => scalaVisitor.visitAbstractType(this)
    case _ =>
  }

  //for allocation-free extractor
  def isEmpty: Boolean = false
  def get: ScAbstractType = this
  def _1: TypeParameter = typeParameter
  def _2: ScType = lower
  def _3: ScType = upper
}

object ScAbstractType {
  def unapply(arg: ScAbstractType): ScAbstractType = arg

  def apply(tp: TypeParameter, lower: ScType, upper: ScType): ScAbstractType = {
    val abstractTp = TypeParameter(tp.psiTypeParameter, tp.typeParameters, lower, upper)
    new ScAbstractType(abstractTp)
  }
}