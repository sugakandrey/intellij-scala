package org.jetbrains.plugins.scala.findUsages.compilerReferences

import java.io.File

import org.jetbrains.jps.incremental.scala.local.LazyCompiledClass
import org.jetbrains.plugin.scala.compilerReferences.BuildData
import org.jetbrains.plugin.scala.compilerReferences.Codec._
import org.junit.Assert._
import org.junit.Test

class BuildDataCodecTest {
  private[this] val sep = "\u0000"
  
  @Test
  def testRoundtrip(): Unit = {
    val fooClass = new LazyCompiledClass(new File("output1"), new File("source1"), "class1")
    val barClass = new LazyCompiledClass(new File("output2"), new File("source2"), "class2")
    
    val data = BuildData(
      123L,
      Set(fooClass, barClass),
      Set("foo", "bar", "baz"),
      Set("moduleFoo"),
      isRebuild = false
    )
    

    val payload = data.encode
    
    val expectedPayload =
      s"123${sep}2${sep}output1${sep}source1${sep}class1${sep}output2${sep}source2${sep}class2${sep}3${sep}foo${sep}bar${sep}baz${sep}1${sep}moduleFoo${sep}false"
    
    assertEquals(expectedPayload, payload)
    
    val deserialized = payload.decode[BuildData]
    assertEquals(Some(data), deserialized)
  }
}
