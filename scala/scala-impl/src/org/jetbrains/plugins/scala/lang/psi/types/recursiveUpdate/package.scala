package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeParameterType
import org.jetbrains.plugins.scala.lang.typeInference.TypeParameter

/**
 * Nikolay.Tropin
 * 01-Feb-18
 */
package object recursiveUpdate {
  type ScSubstitutor    = ScSubstitutorT[ScType]
  type ScalaSubstitutor = ScSubstitutorT[ScalaType]
  type DotSubstitutor   = ScSubstitutorT[DotType]

  val ScSubstitutor: ScSubstitutorT.type = ScSubstitutorT

  implicit class ScSubstitutorExt(private val subst: ScSubstitutor) extends AnyVal {
    def followUpdateThisType(tpe: ScType): ScSubstitutor = {
      subst.assertFullSubstitutor()
      ScSubstitutor(tpe).followed(subst)
    }

    def withBindings(from: Seq[TypeParameter], target: Seq[TypeParameter]): ScSubstitutor = {
      subst.assertFullSubstitutor()

      def simple: ScSubstitutor = ScSubstitutor.bind(from, target)(TypeParameterType(_))

      def mergeHead(old: TypeParamSubstitution): ScSubstitutor = {
        val newMap = TypeParamSubstitution.buildMap(from, target, old.tvMap)(TypeParameterType(_))
        val cloned = subst.substitutions.clone()
        cloned(0) = TypeParamSubstitution(newMap)
        new ScSubstitutor(cloned)
      }

      if (from.isEmpty || target.isEmpty) subst
      else if (subst.isEmpty) simple
      else {
        subst.substitutions.head match {
          case tps: TypeParamSubstitution => mergeHead(tps)
          case _                          => simple.followed(subst)
        }
      }
    }
  }
}
