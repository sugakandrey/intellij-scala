package org.jetbrains.plugins.dotty.lang.core

trait ClassDenotation {
  def name: String
  def qualifiedName: String
  def baseClasses: Seq[ClassDenotation]
}
