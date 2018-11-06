package org.jetbrains.plugins.scala.findUsages.compilerReferences

import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileTypes.FileType
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectFileIndex
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.indices.protocol.sbt.{Configuration, SbtCompilationInfo}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.indices.protocol.CompilationInfo
import org.jetbrains.plugins.scala.indices.protocol.jps.JpsCompilationInfo

private[findUsages] class ScalaDirtyScopeHolder(
  project:        Project,
  fileTypes:      Array[FileType],
  fileIndex:      ProjectFileIndex,
  fileDocManager: FileDocumentManager,
  psiDocManager:  PsiDocumentManager
) extends DirtyScopeHolder[ScalaDirtyScopeHolder.ScopedModule](
      project,
      fileTypes,
      fileIndex,
      fileDocManager,
      psiDocManager
    ) {
  import ScalaDirtyScopeHolder._

  override protected def scopeForSourceContentFile(vfile: VirtualFile): Option[ScopedModule] = {
    val ftype = fileTypeRegistry.getFileTypeByFileName(vfile.getNameSequence)

    inReadAction {
      if (fileTypes.contains(ftype) && fileIndex.isInSourceContent(vfile)) {
        val module = fileIndex.getModuleForFile(vfile).toOption

        module.map(
          m =>
            if (fileIndex.isInTestSourceContent(vfile)) ScopedModule.test(m)
            else ScopedModule.compile(m)
        )
      } else None
    }
  }

  override def moduleScopes(m: Module): Set[ScopedModule] = Set(
    ScopedModule.compile(m),
    ScopedModule.test(m)
  )

  override protected def scopeToSearchScope(scope: ScopedModule): GlobalSearchScope =
    scope.module.getModuleTestsWithDependentsScope

  private[compilerReferences] def compilationInfoIndexed(info: CompilationInfo): Unit = {
    val modules = info.affectedModules(project)

    val scopes = info match {
      case sbti: SbtCompilationInfo => modules.map(ScopedModule(_, sbti.configuration))
      case _: JpsCompilationInfo    => modules.flatMap(moduleScopes)
    }

    scopes.foreach(markScopeUpToDate)
  }
}

object ScalaDirtyScopeHolder {
  final case class ScopedModule(module: Module, configuration: Configuration) {
    override def toString: String = s"${module.getName} / $configuration"
  }

  object ScopedModule {
    def compile(m: Module): ScopedModule = ScopedModule(m, Configuration.Compile)
    def test(m:    Module): ScopedModule = ScopedModule(m, Configuration.Test)
  }
}
