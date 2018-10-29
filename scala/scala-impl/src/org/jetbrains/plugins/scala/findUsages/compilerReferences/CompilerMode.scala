package org.jetbrains.plugins.scala.findUsages.compilerReferences

import com.intellij.openapi.project.Project
import org.jetbrains.sbt.project.settings.SbtProjectSettings
import org.jetbrains.plugins.scala.extensions._

/**
 * Specifies which compiler (e.g. IDEA's own JPS, sbt shell, etc) is used for project builds
 */
sealed trait CompilerMode

object CompilerMode {
  final case object JPS extends CompilerMode
  final case object SBT extends CompilerMode

  def forProject(project: Project): CompilerMode = {
    val sbtSettings  = SbtProjectSettings.forProject(project).toOption
    val usesSbtShell = sbtSettings.exists(_.useSbtShellForBuild)

    if (usesSbtShell) SBT
    else              JPS
  }
}
