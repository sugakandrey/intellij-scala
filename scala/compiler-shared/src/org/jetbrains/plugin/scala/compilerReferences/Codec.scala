package org.jetbrains.plugin.scala.compilerReferences

import java.io._
import java.util.Scanner

import scala.util.Try

import org.jetbrains.jps.incremental.CompiledClass
import org.jetbrains.jps.incremental.scala.local.LazyCompiledClass

trait Codec[T] {
  def encode(t: T): String
  def decode(in: Scanner): Option[T]
}

object Codec {
  private[this] val delimiter = "\u0000"

  def apply[T](implicit codec: Codec[T]): Codec[T] = codec

  implicit class CodecOps[T: Codec](val t: T) {
    def encode: String = Codec[T].encode(t)
  }

  implicit class StringCodecOps(val s: String) extends AnyVal {
    def decode[T: Codec]: Option[T] =
      Codec[T].decode(new Scanner(s))
  }

  def partialCodec[T](decoder: Scanner => T): Codec[T] = new Codec[T] {
    override def encode(t: T): String           = t.toString
    override def decode(in: Scanner): Option[T] = Try(decoder(in)).toOption
  }

  implicit val stringCodec: Codec[String]   = partialCodec(_.next())
  implicit val booleanCodec: Codec[Boolean] = partialCodec(_.next().toBoolean)
  implicit val longCodec: Codec[Long]       = partialCodec(_.next().toLong)
  implicit val intCodec: Codec[Int]         = partialCodec(_.next().toInt)

  implicit def traversableCodec[T, R <: Traversable[T]](
    reify: TraversableOnce[T] => R
  )(implicit codecT: Codec[T]): Codec[R] =
    new Codec[R] {
      override def encode(t: R): String = {
        val length = intCodec.encode(t.size)
        val elements = t.map(codecT.encode)
        length + (if (elements.nonEmpty) elements.mkString(delimiter, delimiter, "") else "")
      }

      override def decode(in: Scanner): Option[R] = {
        in.useDelimiter(delimiter)
        for {
          length <- intCodec.decode(in)
        } yield reify((1 to length).flatMap(_ => codecT.decode(in)))
      }
    }

  implicit def seqCodec[T: Codec]: Codec[Seq[T]] = traversableCodec[T, Seq[T]](_.toSeq)
  implicit def setCodec[T: Codec]: Codec[Set[T]] = traversableCodec[T, Set[T]](_.toSet)

  implicit val compiledClassInfoCodec: Codec[CompiledClass] = new Codec[CompiledClass] {
    override def encode(t: CompiledClass): String =
      Seq(t.getOutputFile.getPath, t.getSourceFile.getPath, t.getClassName).mkString(delimiter)

    override def decode(in: Scanner): Option[CompiledClass] = {
      in.useDelimiter(delimiter)
      for {
        output <- stringCodec.decode(in)
        source <- stringCodec.decode(in)
        name   <- stringCodec.decode(in)
      } yield new LazyCompiledClass(new File(output), new File(source), name)
    }
  }

  implicit val buildDataCodec: Codec[BuildData] = new Codec[BuildData] {
    private[this] val stringSeqCodec = Codec[Seq[String]]
    private[this] val classesCodec   = Codec[Seq[CompiledClass]]

    override def encode(t: BuildData): String =
      Seq(
        t.timeStamp.encode,
        t.compiledClasses.encode,
        t.removedSources.encode,
        t.affectedModules.encode,
        t.isRebuild.encode
      ).mkString(delimiter)

    override def decode(in: Scanner): Option[BuildData] = {
      in.useDelimiter(delimiter)
      for {
        timeStamp       <- longCodec.decode(in)
        compiledClasses <- classesCodec.decode(in)
        removedSources  <- stringSeqCodec.decode(in)
        affectedModules <- stringSeqCodec.decode(in)
        isRebuild       <- booleanCodec.decode(in)
      } yield
        BuildData(
          timeStamp,
          compiledClasses.toSet,
          removedSources.toSet,
          affectedModules.toSet,
          isRebuild
        )
    }
  }
}
