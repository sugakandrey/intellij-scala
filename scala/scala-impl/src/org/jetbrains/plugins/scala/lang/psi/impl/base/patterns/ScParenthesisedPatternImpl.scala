package org.jetbrains.plugins.scala
package lang
package psi
package impl
package base
package patterns

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

/**
* @author ilyas, Alexander Podkhalyuzin
*/

class ScParenthesisedPatternImpl(node: ASTNode) extends ScalaPsiElementImpl (node) with ScPatternImpl with ScParenthesisedPattern {
  override def isIrrefutableFor(t: Option[ScType]): Boolean = innerElement.exists(_.isIrrefutableFor(t))

  override def toString: String = "PatternInParenthesis"

  override def `type`(): TypeResult = innerElement.flatMapType
}
