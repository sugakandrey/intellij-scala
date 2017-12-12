package org.jetbrains.sbt.shell

import com.intellij.util.messages.Topic

trait SbtShellBuildNotificationListener {
  val TOPIC = Topic.create("sbt shell compilation status", classOf[SbtShellBuildNotificationListener])
  
  def compilationFinished()
}
