package org.jetbrains.plugins.scala.findUsages.compilerReferences

import org.jetbrains.jps.backwardRefs.CompilerRef
import org.jetbrains.jps.backwardRefs.CompilerRef.{JavaCompilerFieldRef, JavaCompilerMethodRef}

private class BytecodeReferenceCompilerRefProvider(writer: ScalaCompilerReferenceWriter)
    extends CompilerRefProvider[MemberReference] {

  override def toCompilerRef(ref: MemberReference): CompilerRef = {
    val ownerId = writer.enumerateName(ref.owner)
    val nameId  = writer.enumerateName(ref.name)

    ref match {
      case mref: MethodReference => new JavaCompilerMethodRef(ownerId, nameId, mref.args)
      case _: FieldReference     => new JavaCompilerFieldRef(ownerId, nameId)
    }
  }
}
