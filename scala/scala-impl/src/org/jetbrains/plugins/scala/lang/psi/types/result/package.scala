package org.jetbrains.plugins.scala.lang
package psi
package types

import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.{StdTypes, TypeSystem}
import org.jetbrains.plugins.scala.project.ProjectContext

package object result {

  import scala.util.{Either, Left, Right}

  type TypeResultT[T] = Either[Failure, T]

  type TypeResult      = TypeResultT[ScType]
  type ScalaTypeResult = TypeResultT[ScalaType]
  type DotTypeResult   = TypeResultT[DotType]

  implicit class ScalaTypeResultExt(private val res: ScalaTypeResult) extends AnyVal {
    def asDot: DotTypeResult = res.flatMap {
      case dot: DotType => Right(dot)
      case sc: ScType   => Left(new Failure(s"Expected DotType, but got $sc")(sc.projectContext))
      case _            => ???
    }

    def asSc: TypeResult = res.flatMap {
      case sc: ScType   => Right(sc)
      case dot: DotType => Left(new Failure(s"Expected ScType, but got $dot")(???))
      case _            => ???
    }
  }

  implicit class OptionTypeExt(private val maybeRight: Option[ScType]) extends AnyVal {
    def asTypeResult(implicit context: ProjectContext): TypeResult = maybeRight match {
      case Some(result) => Right(result)
      case None => Failure("")
    }
  }

  implicit class DotTypeResultExt(private val result: DotTypeResult) extends AnyVal {
    def getOrAny(implicit ts: TypeSystem[DotType]): DotType     = getOrDefault(_.Any)
    def getOrNothing(implicit ts: TypeSystem[DotType]): DotType = getOrDefault(_.Nothing)

    private def getOrDefault(
      default: StdTypes[DotType] => DotType
    )(implicit
      ts: TypeSystem[DotType]
    ): DotType = result match {
      case Right(value) => value
      case Left(_)      => default(ts)
    }
  }

  implicit class TypeResultExt(private val result: TypeResult) extends AnyVal {

    def get: ScType = getOrApiType(null)

    def getOrAny: ScType = getOrApiType(_.Any)

    def getOrNothing: ScType = getOrApiType(_.Nothing)

    private def getOrApiType(apiType: ScStdTypes => ScType): ScType = result match {
      case Right(value) => value
      case Left(failure) if apiType != null => apiType(failure.context.stdTypes)
      case _ => throw new NoSuchElementException("Failure.get")
    }
  }

  implicit class TypeableExt[E <: Typeable](private val typeable: Option[E]) extends AnyVal {
    def flatMapTypeResult[Tpe <: ScalaType](
      function: E => TypeResultT[Tpe]
    )(implicit
      ctx: ProjectContext
    ): TypeResultT[Tpe] =
      typeable
        .map(function)
        .getOrElse(Failure("Can't type empty element."))

    def flatMapType(implicit ctx: ProjectContext): TypeResult =
      flatMapTypeResult(_.`type`())
  }
}
