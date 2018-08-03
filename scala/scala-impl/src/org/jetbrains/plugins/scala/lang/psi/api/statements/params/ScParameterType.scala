package org.jetbrains.plugins.scala
package lang
package psi
package api
package statements
package params

import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.types._

/**
* @author Alexander Podkhalyuzin
* Date: 22.02.2008
*/

trait ScParameterType extends ScalaPsiElement {

  def typeElement: ScTypeElement

  def isRepeatedParameter: Boolean = typeElement.isRepeated

  def isCallByNameParameter: Boolean = findChildrenByType(ScalaTokenTypes.tFUNTYPE).nonEmpty
}