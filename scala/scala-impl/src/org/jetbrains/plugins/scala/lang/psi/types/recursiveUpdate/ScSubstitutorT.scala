package org.jetbrains.plugins.scala.lang.psi
package types
package recursiveUpdate

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.util.Key
import org.jetbrains.plugins.scala.extensions.ArrayExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, TypeParamId}
import org.jetbrains.plugins.scala.lang.psi.types.Compatibility.Expression
import org.jetbrains.plugins.scala.lang.psi.types.api.{Covariant, Variance}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.typeInference.Parameter

import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.immutable.LongMap
import scala.util.hashing.MurmurHash3

/**
  * Nikolay.Tropin
  * 01-Feb-18
  */

/** [[ScSubstitutor]] is a transformation of a type which is applied recursively from top to leaves.
  * Examples of such transformation:
  * - replacing type parameter with it's bound or actual argument
  * - replacing `this` type to a concrete inheritor type
  *
  * It's also possible to chain several substitutors to create a compound one.
  *
  * If it's known that every substitution in a chain may update only leaf subtypes, than we may avoid
  * recursively traversing a type several times.
  **/
final class ScSubstitutorT[Tpe <: ScalaType] private[recursiveUpdate] (
  _substitutions: Array[Update[Tpe]], //Array is used for the best concatenation performance, it is effectively immutable
  _fromIndex:     Int = 0
)(implicit
  updater: SubtypeUpdater[Tpe]
) extends (Tpe => Tpe) {

  import ScSubstitutor._

  private[recursiveUpdate] val substitutions = _substitutions
  private[recursiveUpdate] val fromIndex = _fromIndex
  private[recursiveUpdate] def next: ScSubstitutorT[Tpe] = new ScSubstitutorT(substitutions, fromIndex + 1)

  private[recursiveUpdate] lazy val hasNonLeafSubstitutions: Boolean = hasNonLeafSubstitutionsImpl

  private[this] def hasNonLeafSubstitutionsImpl: Boolean = {
    var idx = fromIndex
    while (idx < substitutions.length) {
      if (!substitutions(idx).isInstanceOf[LeafSubstitution[Tpe]])
        return true

      idx += 1
    }
    false
  }

  //positive fromIndex is possible only for temporary substitutors during recursive update
  private[recursiveUpdate] def assertFullSubstitutor(): Unit = LOG.assertTrue(fromIndex == 0)

  override def apply(`type`: Tpe): Tpe = {
    if (cacheSubstitutions)
      cache ++= this.allTypeParamsMap

    recursiveUpdateImpl(`type`)(Set.empty)
  }

  //This method allows application of different `Update` functions in a single pass (see ScSubstitutor).
  //WARNING: If several updates are used, they should be applicable only for leaf types, e.g. which return themselves
  //from `updateSubtypes` method
  @tailrec
  private[recursiveUpdate] def recursiveUpdateImpl(
    tpe:           Tpe,
    variance:      Variance = Covariant,
    isLazySubtype: Boolean = false
  )(implicit
    visited: Set[ScalaType] = Set.empty
  ): Tpe =
    if (fromIndex >= substitutions.length || visited(tpe)) tpe
    else {
      val currentUpdate = substitutions(fromIndex)

      currentUpdate(tpe, variance) match {
        case ReplaceWith(res) =>
          next.recursiveUpdateImpl(res, variance, isLazySubtype)(visited)
        case Stop => tpe
        case ProcessSubtypes =>
          val newVisited = if (isLazySubtype) visited + tpe else visited

          if (hasNonLeafSubstitutions) {
            val withCurrentUpdate =
              updater.noVariance.updateSubtypes(tpe, variance, ScSubstitutorT(currentUpdate))(
                newVisited
              )
            next.recursiveUpdateImpl(withCurrentUpdate, variance)(Set.empty)
          } else {
            updater.noVariance.updateSubtypes(tpe, variance, this)(newVisited)
          }
      }
    }

  def followed(other: ScSubstitutorT[Tpe]): ScSubstitutorT[Tpe] = {
    assertFullSubstitutor()

    if (this.isEmpty)
      if (other.fromIndex > 0) new ScSubstitutorT(other.substitutions)
      else other
    else if (other.isEmpty) this
    else {
      val thisLength = substitutions.length
      val newLength = thisLength + other.substitutions.length

      if (newLength > followLimit)
        LOG.error("Too much followers for substitutor: " + this.toString)

      val newArray = new Array[Update[Tpe]](newLength)
      substitutions.copyToArray(newArray, 0)
      other.substitutions.copyToArray(newArray, thisLength)

      new ScSubstitutorT(newArray)
    }
  }

  private[recursiveUpdate] def narrow[T <: Tpe](implicit updater: SubtypeUpdater[T]): ScSubstitutorT[T] =
    new ScSubstitutorT[T](substitutions.map(_.narrow[T]), fromIndex)

  override def hashCode(): Int = MurmurHash3.arrayHash(substitutions)

  override def equals(obj: Any): Boolean = obj match {
    case other: ScSubstitutorT[Tpe] => other.substitutions sameElements substitutions
    case _                          => false
  }

  override def toString: String = {
    val text = substitutions.mkString(" >> ")
    s"ScSubstitutor($text)"
  }

  def isEmpty: Boolean = substitutions.isEmpty

  private def allTypeParamsMap: LongMap[ScType] = substitutions.foldLeft(LongMap.empty[ScType]) {
    (map, substitution) =>
      substitution match {
        case tps: TypeParamSubstitution => map ++ tps.tvMap
        case _                          => map
      }
  }
}

object ScSubstitutorT {
  val LOG: Logger = Logger.getInstance("#org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor")

  val key: Key[ScSubstitutor] = Key.create("scala substitutor key")

  val empty: ScSubstitutor = new ScSubstitutor(Array.empty)

  private[recursiveUpdate] def apply[Tpe <: ScalaType](
    s: Update[Tpe]
  )(implicit
    updater: SubtypeUpdater[Tpe]
  ): ScSubstitutorT[Tpe] =
    new ScSubstitutorT(Array(s))

  private val followLimit = 800

  var cacheSubstitutions = false

  var cache: LongMap[ScType] = LongMap.empty

  def apply(tvMap: LongMap[ScType]): ScSubstitutor = {
    if (tvMap.isEmpty) ScSubstitutor.empty
    else ScSubstitutorT(TypeParamSubstitution(tvMap))
  }

  def apply(updateThisType: ScType): ScSubstitutor =
    ScSubstitutorT(ThisTypeSubstitution(updateThisType))

  def paramToExprType(parameters: Seq[Parameter], expressions: Seq[Expression], useExpected: Boolean = true) =
    ScSubstitutor(ParamsToExprs(parameters, expressions, useExpected))

  def paramToParam(fromParams: Seq[ScParameter], toParams: Seq[ScParameter]) =
    ScSubstitutor(ParamToParam(fromParams, toParams))

  def paramToType(fromParams: Seq[Parameter], types: Seq[ScType]) =
    ScSubstitutor(ParamToType(fromParams, types))

  def bind[T: TypeParamId](typeParamsLike: Seq[T])(toScType: T => ScType): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, typeParamsLike)(toScType)
    ScSubstitutor(tvMap)
  }

  def bind[T: TypeParamId, S](typeParamsLike: Seq[T], targets: Seq[S])(toScType: S => ScType): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, targets)(toScType)
    ScSubstitutor(tvMap)
  }

  def bind[T: TypeParamId](typeParamsLike: Seq[T], types: Seq[ScType]): ScSubstitutor = {
    val tvMap = TypeParamSubstitution.buildMap(typeParamsLike, types)(identity)
    ScSubstitutor(tvMap)
  }

  def updateThisTypeDeep(subst: ScSubstitutor): Option[ScType] = {
    subst.substitutions.collectFirstByType[ThisTypeSubstitution, ScType](_.target)
  }
}