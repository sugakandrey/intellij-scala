//package org.jetbrains.plugins.scala.findUsages.backwardRefs
//
//import java.io.IOException
//
//import org.jetbrains.jps.ModuleChunk
//import org.jetbrains.jps.builders.DirtyFilesHolder
//import org.jetbrains.jps.builders.java.JavaSourceRootDescriptor
//import org.jetbrains.jps.incremental.ModuleLevelBuilder.ExitCode
//import org.jetbrains.jps.incremental.{CompileContext, CompiledClass, ModuleBuildTarget, ModuleLevelBuilder}
//
//import java.util
//
//import scala.collection.JavaConverters._
//
//class ScalaBackwardReferenceIndexBuilder extends BackwardReferenceIndexBuilder {
//  override def getPresentableName: String = "Scala backward-references indexer"
//
//  private def writeFileData(compiledClass: CompiledClass, writer: BackwardReferenceIndexWriter): Unit = {
//    val constantPool     = ConstantPoolParser.parse(compiledClass.getOutputFile)
//    val javacRefProvider = new ConstantPoolJavacRefProvider(constantPool)
//    val javacRefs = constantPool.getConstantPool.collect {
//      case javacRefProvider(ref) => writer.enumerateNames(ref, Function.const(null)) // Such FP
//    }
//    val sourceFile = compiledClass.getSourceFiles.asScala.head
//    val fileId     = writer.enumeratePath(sourceFile.getPath)
//    writer.writeData(fileId, new ConstantPoolData(javacRefs))
//  }
//
//  private def processCompiledClasses(
//    classes: util.Collection[CompiledClass],
//    writer: BackwardReferenceIndexWriter
//  ): Unit =
//    try classes.forEach(writeFileData(_, writer))
//    catch { case e: IOException => writer.setRebuildCause(e) }
//
//  override def build(
//    context: CompileContext,
//    chunk: ModuleChunk,
//    dirtyFilesHolder: DirtyFilesHolder[JavaSourceRootDescriptor, ModuleBuildTarget],
//    outputConsumer: ModuleLevelBuilder.OutputConsumer
//  ): ExitCode = {
//    val writer = BackwardReferenceIndexWriter.getInstance()
//    if (BackwardReferenceIndexWriter.isEnabled && writer != null) {
//      processCompiledClasses(outputConsumer.getCompiledClasses.values(), writer)
//    }
//    null
//  }
//}
