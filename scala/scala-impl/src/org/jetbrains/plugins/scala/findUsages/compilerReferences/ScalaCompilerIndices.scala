package org.jetbrains.plugins.scala.findUsages.compilerReferences

import java.io.{DataInput, DataOutput}
import java.util

import com.intellij.openapi.util.io.{DataInputOutputUtilRt => ioutil}
import com.intellij.util.indexing.{DataIndexer, IndexExtension, IndexId}
import com.intellij.util.io.{DataExternalizer, KeyDescriptor}
import org.jetbrains.jps.backwardRefs.CompilerRef
import org.jetbrains.jps.backwardRefs.CompilerRef._

import scala.collection.JavaConverters._

private object ScalaCompilerIndices {
  val backwardUsages: IndexId[CompilerRef, Seq[Int]]            = IndexId.create("sc.back.refs")
  val backwardHierarchy: IndexId[CompilerRef, Seq[CompilerRef]] = IndexId.create("sc.back.hierarchy")
  val version                                                   = 0

  val getIndices: util.Collection[_ <: IndexExtension[_, _, _ >: CompiledScalaFile]] = util.Arrays.asList(
    backUsagesExtension,
    backHierarchyExtension
  )

  private[this] val refDescriptor = new KeyDescriptor[CompilerRef] {
    override def getHashCode(t: CompilerRef): Int                  = t.hashCode
    override def isEqual(t: CompilerRef, t1: CompilerRef): Boolean = t == t1
    override def save(out:  DataOutput, t:   CompilerRef): Unit    = t.save(out)

    override def read(in: DataInput): CompilerRef = in.readByte() match {
        case 0                                    => new JavaCompilerClassRef(ioutil.readINT(in))
        case 1                                    => new JavaCompilerMethodRef(ioutil.readINT(in), ioutil.readINT(in), ioutil.readINT(in))
        case 2                                    => new JavaCompilerFieldRef(ioutil.readINT(in), ioutil.readINT(in))
        case 3                                    => new JavaCompilerFunExprDef(ioutil.readINT(in))
        case 4                                    => new JavaCompilerAnonymousClassRef(ioutil.readINT(in))
        case ScFunExprCompilerRef.ScFunExprMarker => ScFunExprCompilerRef(ioutil.readINT(in))
      }
  }

  private[this] def backUsagesExtension: IndexExtension[CompilerRef, Seq[Int], CompiledScalaFile] =
    new IndexExtension[CompilerRef, Seq[Int], CompiledScalaFile] {
      override def getVersion: Int                                                   = version
      override def getName: IndexId[CompilerRef, Seq[Int]]                           = backwardUsages
      override def getIndexer: DataIndexer[CompilerRef, Seq[Int], CompiledScalaFile] = _.refs
      override def getKeyDescriptor: KeyDescriptor[CompilerRef]                      = refDescriptor
      override def getValueExternalizer: DataExternalizer[Seq[Int]]                  = seqExternalizer(ioutil.readINT, ioutil.writeINT)
    }

  private[this] def backHierarchyExtension: IndexExtension[CompilerRef, Seq[CompilerRef], CompiledScalaFile] =
    new IndexExtension[CompilerRef, Seq[CompilerRef], CompiledScalaFile] {
      override def getVersion: Int                                                           = version
      override def getName: IndexId[CompilerRef, Seq[CompilerRef]]                           = backwardHierarchy
      override def getIndexer: DataIndexer[CompilerRef, Seq[CompilerRef], CompiledScalaFile] = _.backwardHierarchy
      override def getKeyDescriptor: KeyDescriptor[CompilerRef]                              = refDescriptor
      override def getValueExternalizer: DataExternalizer[Seq[CompilerRef]] =
        seqExternalizer(refDescriptor.read, refDescriptor.save)
    }

  private[this] def seqExternalizer[T](
    reader: DataInput => T,
    writer: (DataOutput, T) => Unit
  ): DataExternalizer[Seq[T]] = new DataExternalizer[Seq[T]] {
    override def read(in:  DataInput): Seq[T] = ioutil.readSeq[T](in, () => reader(in)).asScala
    override def save(out: DataOutput, xs: Seq[T]): Unit = ioutil.writeSeq(out, xs.asJava, writer(out, _: T))
  }
}
