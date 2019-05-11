package org.jetbrains.plugins.scala.lang.psi.types

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.dotty.lang.psi.types.DottyTypeSystem
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameter
import org.jetbrains.plugins.scala.project.ProjectContext

/**
  * @author adkozlov
  */
package object api {

  implicit class TypeSystemExt(private val ts: TypeSystem[_]) extends AnyVal {
    def isDotty: Boolean = ts match {
      case _: DottyTypeSystem => true
      case _                  => false
    }

    def isScala: Boolean = ts match {
      case _: ScalaTypeSystem => true
      case _                  => false
    }
  }

  implicit class TypeParametersArrayExt(val typeParameters: Array[TypeParameter]) extends AnyVal {
    def depth: Int = typeParameters.toSeq.depth
  }

  implicit class TypeParametersSeqExt(val typeParameters: Seq[TypeParameter]) extends AnyVal {
    def depth: Int = {
      def depth(tp: TypeParameter): Int = Seq(tp.lowerType.typeDepth, tp.upperType.typeDepth, tp.typeParameters.depth).max

      val maxDepth = if (typeParameters.isEmpty) 0 else typeParameters.map(depth).max
      1 + maxDepth
    }
  }

  implicit class PsiTypeParamatersExt(val typeParameters: Array[PsiTypeParameter]) extends AnyVal {
    def instantiate: Seq[TypeParameter] = typeParameters match {
      case Array() => Seq.empty
      case array => array.toSeq.map(TypeParameter(_))
    }
  }

  def Any(implicit pc: ProjectContext) = ScStdTypes.instance.Any

  def AnyRef(implicit pc: ProjectContext) = ScStdTypes.instance.AnyRef

  def Null(implicit pc: ProjectContext) = ScStdTypes.instance.Null

  def Nothing(implicit pc: ProjectContext) = ScStdTypes.instance.Nothing

  def Singleton(implicit pc: ProjectContext) = ScStdTypes.instance.Singleton

  def AnyVal(implicit pc: ProjectContext) = ScStdTypes.instance.AnyVal

  def Unit(implicit pc: ProjectContext) = ScStdTypes.instance.Unit

  def Boolean(implicit pc: ProjectContext) = ScStdTypes.instance.Boolean

  def Char(implicit pc: ProjectContext) = ScStdTypes.instance.Char

  def Byte(implicit pc: ProjectContext) = ScStdTypes.instance.Byte

  def Short(implicit pc: ProjectContext) = ScStdTypes.instance.Short

  def Int(implicit pc: ProjectContext) = ScStdTypes.instance.Int

  def Long(implicit pc: ProjectContext) = ScStdTypes.instance.Long

  def Float(implicit pc: ProjectContext) = ScStdTypes.instance.Float

  def Double(implicit pc: ProjectContext) = ScStdTypes.instance.Double

  val Bivariant     = Variance.Bivariant
  val Covariant     = Variance.Covariant
  val Contravariant = Variance.Contravariant
  val Invariant     = Variance.Invariant
}
