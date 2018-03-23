package org.jetbrains.plugin.scala.compilerReferences

import java.io.{ByteArrayOutputStream, DataInput, DataOutput, DataOutputStream}
import java.util.Scanner

import com.intellij.util.io.IOUtil
import org.jetbrains.jps.incremental.messages.CustomBuilderMessage
import org.jetbrains.plugin.scala.compilerReferences.Codec._

import scala.collection.JavaConverters._

object BuilderMessages {
  import CompilerReferenceIndexBuilder.{buildDataType, id}
  
  final case class TimeStamped[T: Codec](value: T, timeStamp: Long)
  
  implicit def timeStampedCodec[T: Codec](v: TimeStamped[T]): Codec[TimeStamped[T]] =
    seqLikeCodec[String, TimeStamped[T]]()(t => Seq(t.timeStamp))

  sealed abstract class TimestampedMessage[T: Codec](payload: T)
      extends CustomBuilderMessage(id, buildDataType, TimeStamped(payload).encode)
}
