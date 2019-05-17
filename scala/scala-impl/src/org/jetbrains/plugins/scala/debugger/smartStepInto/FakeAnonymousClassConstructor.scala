package org.jetbrains.plugins.scala.debugger.smartStepInto

import java.util.Objects

import javax.swing.Icon
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.fake.FakePsiMethod
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.Unit
import org.jetbrains.plugins.scala.lang.typeInference.Parameter
/**
 * @author Nikolay.Tropin
 */
class FakeAnonymousClassConstructor(templ: ScNewTemplateDefinition, interfaceName: String)
  extends FakePsiMethod(templ, Some(templ), interfaceName) {
  override def isConstructor: Boolean = true

  override def getIcon(flags: Int): Icon = Icons.CLASS

  override def equals(obj: scala.Any): Boolean = obj match {
    case fake: FakeAnonymousClassConstructor =>
      fake.getOriginalElement == this.getOriginalElement && fake.getName == this.getName
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(templ, getName)

  def params: Array[Parameter] = Array.empty

  def retType: ScType = Unit(templ.projectContext)
}

