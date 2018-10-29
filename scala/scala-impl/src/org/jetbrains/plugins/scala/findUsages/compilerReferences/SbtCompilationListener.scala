package org.jetbrains.plugins.scala.findUsages.compilerReferences

import java.nio.file.Path
import java.util.{EventListener, UUID}

import com.intellij.util.messages.Topic
import org.jetbrains.plugins.scala.findUsages.compilerReferences.SbtCompilationListener.ProjectIdentifier.ProjectBase

/**
 * Low level listener component providing access to raw TCP socket events from sbt compilations
 * see also [[SbtCompilationWatcher]]
 */
trait SbtCompilationListener extends EventListener {
  import SbtCompilationListener._

  def beforeCompilationStart(project: ProjectBase, compilationId: UUID): Unit          = ()
  def connectionFailure(project: ProjectIdentifier, compilationId: Option[UUID]): Unit = ()

  def onCompilationSuccess(
    project:             ProjectBase,
    compilationId:       UUID,
    compilationInfoFile: String
  ): Unit = ()

  def onCompilationFailure(project: ProjectBase, compilationId: UUID): Unit = ()
}

object SbtCompilationListener {
  trait ProjectIdentifier
  object ProjectIdentifier {
    final case class ProjectBase(path: Path) extends ProjectIdentifier
    final object Unidentified                extends ProjectIdentifier
  }

  val topic: Topic[SbtCompilationListener] =
    Topic.create[SbtCompilationListener]("sbt compilation status", classOf[SbtCompilationListener])
}
