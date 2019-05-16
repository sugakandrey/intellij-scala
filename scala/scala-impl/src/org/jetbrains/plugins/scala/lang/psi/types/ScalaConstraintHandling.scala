package org.jetbrains.plugins.scala.lang.psi.types

import com.intellij.openapi.util.Ref
import org.jetbrains.plugins.scala.extensions.{IteratorExt, ObjectExt}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.TypeParamId
import org.jetbrains.plugins.scala.lang.psi.types.api._
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate._
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.{ScSubstitutor, ScSubstitutorT}

import scala.collection.immutable.LongMap

trait ScalaConstraintHandling extends api.ConstraintHandling[ScType] {
  this: TypeSystem[ScType] =>

  import ScalaConstraintHandling._

  override def emptyConstraints: ConstraintSystem[ScType] = ConstraintSystemImpl(
    LongMap.empty,
    LongMap.empty,
    Set.empty
  )

  override def multiConstraintSystem(
    constraints: Set[ConstraintSystem[ScType]]
  ): ConstraintSystem[ScType] =
    ScalaConstraintSystem.apply(constraints)

  object ScalaConstraintSystem {
    def apply(constraintsSet: Set[ConstraintSystem[ScType]]): ConstraintSystem[ScType] = {
      val flattened = constraintsSet.filterNot {
        _.isEmpty
      }.flatMap {
        case impl: ConstraintSystemImpl => Set(impl)
        case MultiConstraintSystem(impls) => impls
      }

      flattened.size match {
        case 0 => emptyConstraints
        case 1 => flattened.head
        case _ => MultiConstraintSystem(flattened)
      }
    }
  }

  private final case class ScSubstitutionBounds(
    override val tvMap:    LongMap[ScType],
    override val lowerMap: LongMap[ScType],
    override val upperMap: LongMap[ScType]
  ) extends SubstitutionBounds[ScType](tvMap, lowerMap, upperMap) {
    override def substitutor: ScSubstitutorT[ScType] = ScSubstitutor(tvMap)
  }


  private final case class ConstraintSystemImpl(
    upperMap:      LongMap[Set[ScType]],
    lowerMap:      LongMap[Set[ScType]],
    additionalIds: Set[Long]
  ) extends ConstraintSystem[ScType] {

    import ConstraintSystemImpl._

    private[this] var substWithBounds: Option[SubstitutionBounds[ScType]] = _

    private[this] var substWithBoundsNoSCE: Option[SubstitutionBounds[ScType]] = _

    override def isApplicable(id: Long): Boolean =
      upperMap.contains(id) || lowerMap.contains(id)

    override def isEmpty: Boolean = upperMap.isEmpty && lowerMap.isEmpty

    override def +(constraints: ConstraintSystem[ScType]): ConstraintSystem[ScType] =
      constraints match {
        case ConstraintSystemImpl(otherUpperMap, otherLowerMap, otherAdditionalIds) =>
          ConstraintSystemImpl(
            upperMap.merge(otherUpperMap)(isAny),
            lowerMap.merge(otherLowerMap)(isNothing),
            additionalIds ++ otherAdditionalIds
          )
        case multi: MultiConstraintSystem => multi + this
      }

    override def withTypeParamId(id: Long): ConstraintSystem[ScType] = copy(
      additionalIds = additionalIds + id
    )

    override def withLower(
      id:       Long,
      rawLower: ScType,
      variance: Variance
    ): ConstraintSystem[ScType] =
      computeLower(variance, rawLower) match {
        case None        => this
        case Some(lower) => copy(lowerMap = lowerMap.update(id, lower))
      }

    override def withUpper(
      id:       Long,
      rawUpper: ScType,
      variance: Variance
    ): ConstraintSystem[ScType] =
      computeUpper(variance, rawUpper) match {
        case None        => this
        case Some(upper) => copy(upperMap = upperMap.update(id, upper))
      }

    override def substitutionBounds(canThrowSCE: Boolean): Option[SubstitutionBounds[ScType]] = {
      def init(
        get: =>Option[SubstitutionBounds[ScType]]
      )(set: Option[SubstitutionBounds[ScType]] => Unit
      ) = get match {
        case null =>
          val value = substitutionBoundsImpl(canThrowSCE)
          set(value)
          value
        case value => value
      }

      if (canThrowSCE) init(substWithBounds)(substWithBounds = _)
      else init(substWithBoundsNoSCE)(substWithBoundsNoSCE = _)
    }

    override def removeTypeParamIds(ids: Set[Long]): ConstraintSystem[ScType] = copy(
      upperMap = upperMap.removeIds(ids),
      lowerMap = lowerMap.removeIds(ids)
    )

    private def substitutionBoundsImpl(canThrowSCE: Boolean): Option[SubstitutionBounds[ScType]] = {
      var tvMap = LongMap.empty[ScType]
      var lMap = LongMap.empty[ScType]
      var uMap = LongMap.empty[ScType]

      def solve(visited: Set[Long])(id: Long): Boolean = {
        if (visited.contains(id)) {
          tvMap += ((id, Nothing))
          return false
        }

        tvMap.contains(id) || {
          val needTvMap = {
            val newVisited = visited + id
            recursion(!solve(newVisited)(_) && canThrowSCE) _
          }

          lowerMap.getOrDefault(id) match {
            case set if set.nonEmpty =>
              val substitutor = needTvMap(set).fold {
                tvMap += ((id, Nothing))
                return false
              } {
                case true => ScSubstitutor(tvMap)
                case _    => ScSubstitutor.empty
              }

              val lower = set.map(substitutor).reduce {
                _.lub(_)
              }

              lMap  += ((id, lower))
              tvMap += ((id, lower))
            case _ =>
          }

          upperMap.getOrDefault(id) match {
            case set if set.nonEmpty =>
              val substitutor = needTvMap(set).fold {
                tvMap += ((id, Nothing))
                return false
              } {
                case true => ScSubstitutor(tvMap)
                case _    => ScSubstitutor.empty
              }

              val upper = set.map(substitutor).reduce {
                _.glb(_)
              }
              uMap += ((id, upper))

              tvMap.get(id) match {
                case Some(lower) =>
                  if (canThrowSCE && !lower.conforms(upper)) {
                    return false
                  }
                case _ => tvMap += ((id, upper))
              }
            case _ =>
          }

          if (tvMap.get(id).isEmpty) {
            tvMap += ((id, Nothing))
          }
          tvMap.contains(id)
        }
      }

      for ((id, _) <- upperMap.iterator ++ lowerMap.iterator) {
        if (!solve(Set.empty)(id) && canThrowSCE) return None
      }

      Some(ScSubstitutionBounds(tvMap, lMap, uMap))
    }

    private def recursion(break: Long => Boolean)(set: Set[ScType]): Option[Boolean] = {
      def predicate(flag: Ref[Boolean])(`type`: ScType): Boolean = {
        def innerBreak[T](owner: T)(visited: Long => Boolean)(implicit evidence: TypeParamId[T]) =
          evidence.typeParamId(owner) match {
            case id if visited(id) =>
              flag.set(true)
              break(id)
            case _ => false
          }

        `type`.visitRecursively {
          case tpt: TypeParameterType if innerBreak(tpt)(additionalIds.contains) => return false
          case UndefinedType(tp, _) if innerBreak(tp)(isApplicable)              => return false
          case _                                                                 =>
        }

        true
      }

      Ref.create(false) match {
        case needTvMap if set.forall(predicate(needTvMap)) => Some(needTvMap.get)
        case _                                             => None
      }
    }
  }

  private object ConstraintSystemImpl {
    private def computeUpper(variance: Variance, rawUpper: ScType): Option[ScType] =
      updateUpper(variance, rawUpper).unpackedType
        .ifNot(isAny)

    private def computeLower(variance: Variance, rawLower: ScType): Option[ScType] =
      updateLower(variance, rawLower).unpackedType
        .ifNot(isNothing)

    private def isAny(`type`: ScType) =
      `type`.equiv(Any)

    private def isNothing(`type`: ScType) =
      `type`.equiv(Nothing)

    private[this] def updateUpper(
      variance: Variance,
      rawUpper: ScType
    )(implicit
      freshExArg: FreshExistentialArg
    ): ScType =
      rawUpper match {
        case ScAbstractType(_, _, upper) if variance == Invariant                 => upper
        case ScAbstractType(_, _, upper) if variance == Covariant && isAny(upper) => Any
        case _ =>
          recursiveVarianceUpdate(rawUpper, variance)(
            invariantAbstract = freshExArg(_), // TODO: why this is right?
            invariantExistentialArg = freshExArg(_)
          )
      }

    private[this] def updateLower(
      variance: Variance,
      rawLower: ScType
    )(implicit
      freshExArg: FreshExistentialArg
    ): ScType =
      rawLower match {
        case ScAbstractType(_, lower, _)                       => lower
        case ex: ScExistentialArgument if variance.isInvariant => freshExArg(ex)
        case _ =>
          recursiveVarianceUpdate(rawLower, -variance.sign)(
            invariantAbstract = _.lower, // TODO: why this is right?
            invariantExistentialArg = freshExArg(_)
          )
      }

    private[this] def recursiveVarianceUpdate(
      `type`:                  ScType,
      variance:                Variance,
      revertVariances:         Boolean = false
    )(invariantAbstract:       ScAbstractType => ScType,
      invariantExistentialArg: ScExistentialArgument => ScType
    ) =
      `type`.recursiveVarianceUpdate(variance) {
        case (a: ScAbstractType, newVariance) =>
          replaceAbstractType(newVariance, a)(invariantAbstract)
        case (ex: ScExistentialArgument, newVariance) =>
          replaceExistentialArg(newVariance, ex)(invariantExistentialArg)
        case (_: ScExistentialType, _) => Stop
        case _                         => ProcessSubtypes
      }

    private[this] def replaceAbstractType(
      variance:      Variance,
      a:             ScAbstractType
    )(invariantCase: ScAbstractType => ScType
    ) = ReplaceWith {
      variance match {
        case Contravariant => a.lower
        case Covariant     => a.upper
        case Invariant     => invariantCase(a)
      }
    }

    private[this] def replaceExistentialArg(
      variance:      Variance,
      ex:            ScExistentialArgument
    )(invariantCase: ScExistentialArgument => ScType
    ) = ReplaceWith {
      variance match {
        case Contravariant => ex.lower
        case Covariant     => ex.upper
        case Invariant     => invariantCase(ex)
      }
    }

    private implicit def freshExistentialArg: FreshExistentialArg = new FreshExistentialArg

    private class FreshExistentialArg {
      private[this] var index = 0

      def apply(a: ScAbstractType): ScExistentialArgument = {
        index += 1
        ScExistentialArgument(s"_$$$index", Nil, a.lower, a.upper)
      }

      def apply(e: ScExistentialArgument): ScExistentialArgument = {
        index += 1
        ScExistentialArgument(s"_$$$index", Nil, e.lower, e.upper)
      }
    }
  }
  private final case class MultiConstraintSystem(impls: Set[ConstraintSystemImpl])
  extends ConstraintSystem[ScType] {

  override def isApplicable(id: Long): Boolean = impls.exists {
    _.isApplicable(id)
  }

  override def isEmpty: Boolean = impls.forall {
    _.isEmpty
  }

  override def withTypeParamId(id: Long): ConstraintSystem[ScType] = map {
    _.withTypeParamId(id)
  }

  override def withLower(id: Long, lower: ScType, variance: Variance): ConstraintSystem[ScType] = map {
    _.withLower(id, lower, variance)
  }

  override def withUpper(id: Long, upper: ScType, variance: Variance): ConstraintSystem[ScType] = map {
    _.withUpper(id, upper, variance)
  }

  override def substitutionBounds(canThrowSCE: Boolean): Option[SubstitutionBounds[ScType]] =
    impls.iterator.flatMap {
      _.substitutionBounds(canThrowSCE)
    }.headOption

  override def removeTypeParamIds(ids: Set[Long]): ConstraintSystem[ScType] = map {
    _.removeTypeParamIds(ids)
  }

  override def +(constraints: ConstraintSystem[ScType]): ConstraintSystem[ScType] = {
    val otherImpls = constraints match {
      case impl: ConstraintSystemImpl => Set(impl)
      case MultiConstraintSystem(otherSubstitutors) => otherSubstitutors
    }

    ScalaConstraintSystem {
      for {
        left <- impls
        right <- otherImpls
      } yield left + right
    }
  }

  private def map(function: ConstraintSystemImpl => ConstraintSystem[ScType]): ConstraintSystem[ScType] =
    ScalaConstraintSystem(impls.map(function))
  }
}

object ScalaConstraintHandling {
  private implicit class LongMapExt(val map: LongMap[Set[ScType]]) extends AnyVal {
    def getOrDefault(id: Long): Set[ScType] = map.getOrElse(id, Set.empty)

    def removeIds(set: Set[Long]): LongMap[Set[ScType]] = map.filterNot {
      case (long, _) => set(long)
    }

    def update(id: Long, `type`: ScType): LongMap[Set[ScType]] =
      map.updated(id, getOrDefault(id) + `type`)

    def merge(map: LongMap[Set[ScType]])(predicate: ScType => Boolean): LongMap[Set[ScType]] =
      this.map.unionWith(map, (_, left, right) => left ++ right).modifyOrRemove { (_, set) =>
        set.filterNot(predicate) match {
          case filtered if filtered.nonEmpty => Some(filtered)
          case _                             => None
        }
      }
  }

  object Constraints {
    object withSubstitutor {
      def unapply(system: ScConstraintSystem): Option[ScSubstitutor] =
        system.substitutionBounds(canThrowSCE = true).map(_.substitutor)
    }
  }
}
