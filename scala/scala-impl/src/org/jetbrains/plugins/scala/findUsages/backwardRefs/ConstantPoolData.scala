package org.jetbrains.plugins.scala.findUsages.backwardRefs

import java.util
import java.util.Collections

import org.jetbrains.jps.backwardRefs.LightRef
import org.jetbrains.jps.backwardRefs.index.CompiledFileData

class ConstantPoolData(refs: Seq[LightRef]) extends {
  val refMap = new util.HashMap[LightRef, Integer](refs.size)  {{
    refs.foreach(put(_, 1))
  }}
} with CompiledFileData(
  Collections.emptyMap(),
  Collections.emptyMap(),
  refMap,
  Collections.emptyMap(),
  Collections.emptyMap()
)
