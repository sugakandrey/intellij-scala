package org.jetbrains.plugins.scala.findUsages.backwardRefs

import java.io.{ByteArrayInputStream, DataInputStream, File, FileInputStream}

import org.apache.bcel.classfile.ConstantPool

object ConstantPoolParser {
  def parse(is: DataInputStream): ConstantPool = {
    is.skipBytes(4 + 2 + 2) // skip magic and minor/major version bytes
    new ConstantPool(is)
  }

  def parse(bytes: Array[Byte]): ConstantPool = parse(new DataInputStream(new ByteArrayInputStream(bytes)))
  def parse(file: File): ConstantPool         = parse(new DataInputStream(new FileInputStream(file)))
}
