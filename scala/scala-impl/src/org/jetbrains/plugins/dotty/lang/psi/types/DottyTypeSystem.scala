package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.openapi.roots.ProjectRootManager
import org.jetbrains.plugins.dotty.lang.core.types.DotType
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeSystem
import org.jetbrains.plugins.scala.macroAnnotations.CachedInUserData
import org.jetbrains.plugins.scala.project.ProjectContext

final class DottyTypeSystem private (implicit val projectContext: ProjectContext)
    extends TypeSystem[DotType]
    with DottyEquivalence
    with DottyConformance
    with DottyBounds
    with DottyPsiTypeBridge
    with DottyTypePresentation
    with DottyStdTypes
    with DottyTyper
    with DottyConstraintHandling {

  override val name = "Dotty"
}

object DottyTypeSystem {
  def instance(implicit projectContext: ProjectContext): DottyTypeSystem = {

    @CachedInUserData(projectContext.project, ProjectRootManager.getInstance(projectContext))
    def cached: DottyTypeSystem = new DottyTypeSystem

    cached
  }
}
