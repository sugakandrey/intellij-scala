package org.jetbrains.plugins.scala.lang.transformation
package calls

import org.jetbrains.plugins.scala.extensions.FirstChild
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlockExpr, ScInfixExpr}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode._

/**
  * @author Pavel Fatin
  */
object CanonizeInfixCall extends AbstractTransformer {
  def transformation = {
    case e @ ScInfixExpr(l, FirstChild(o), r) =>
      val (a, b) = if (o.getText.endsWith(":")) (r, l) else (l, r)

      val element = b match {
        case block: ScBlockExpr => code"$a.$o {${block.exprs}}"
        case _ => code"$a.$o($b)"
      }

      e.replace(element)
  }
}
