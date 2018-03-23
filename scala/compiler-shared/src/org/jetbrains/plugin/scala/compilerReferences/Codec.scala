package org.jetbrains.plugin.scala.compilerReferences

import java.io._

import com.intellij.openapi.util.io.DataInputOutputUtilRt.readSeq
import org.jetbrains.jps.incremental.CompiledClass
import org.jetbrains.jps.incremental.scala.local.LazyCompiledClass

import scala.collection.JavaConverters._
import scala.util.Try

trait Codec[T] {
  def encode(t: T): String
  def decode(in: DataInput): Option[T]
}

object Codec {
  def apply[T](implicit codec: Codec[T]): Codec[T] = codec

  implicit class CodecOps[T: Codec](val t: T) {
    def encode: String = Codec[T].encode(t)
  }

  implicit class StringCodecOps(val s: String) extends AnyVal {
    def decode[T: Codec]: Option[T] =
      Codec[T].decode(new DataInputStream(new ByteArrayInputStream(s.getBytes())))
  }

  def partialCodec[T](decoder: DataInput => T): Codec[T] = new Codec[T] {
    override def encode(t: T): String = t.toString
    override def decode(in: DataInput): Option[T] = Try(decoder(in)).toOption
  }

  implicit val stringCodec: Codec[String] = partialCodec(_.readUTF())
  implicit val booleanCodec: Codec[Boolean] = partialCodec(_.readBoolean())
  implicit val longCodec: Codec[Long] = partialCodec(_.readLong)
  implicit val intCodec: Codec[Int] = partialCodec(_.readInt())

  implicit def seqCodec[T](implicit codecT: Codec[T]): Codec[Seq[T]] =
    new Codec[Seq[T]] {
      override def encode(t: Seq[T]): String = {
        val elements = t.map(codecT.encode)
        val baos     = new ByteArrayOutputStream()
        val out      = new DataOutputStream(baos)
        out.writeInt(elements.length)
        elements.foreach(out.writeUTF)
        baos.toString
      }

      override def decode(in: DataInput): Option[R] =
        Try(readSeq[T](in, () => in.readUTF().decode[T].get).asScala).flatMap(fromSeq).toOption
    }

  implicit val compiledClassInfoCodec: Codec[CompiledClass] = seqLikeCodec(){
    private[this] val seqCodec = Codec[Seq[String]]

    override def encode(t: CompiledClass): String =
      seqCodec.encode(Seq(t.getOutputFile.getPath, t.getSourceFile.getPath, t.getClassName))

    override def decode(in: DataInput): Option[CompiledClass] = seqCodec.decode(in).collect {
      case Seq(output, source, name) => new LazyCompiledClass(new File(output), new File(source), name)
    }
  }
}
