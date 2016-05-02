package org.jetbrains.plugins.scala.lang.transformation
package calls

import org.jetbrains.plugins.scala.extensions.ReferenceTarget
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScMethodCall
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode._

/**
  * @author Pavel Fatin
  */
object ExpandVarargArgument extends AbstractTransformer {
  // TODO improve array detection
  def transformation = {
    case e @ ScMethodCall(r @ ReferenceTarget(f: ScFunctionDefinition), as)
      if f.parameters.exists(_.isRepeatedParameter) &&
        !r.text.contains("Array") &&
        !e.args.matchedParameters.exists(p => p._2.isRepeated && p._1.text.contains("Array")) =>

      val as2 = e.matchedParameters.filter(_._2.isRepeated).map(_._1).sortBy(_.getStartOffsetInParent)
      val as1 = as.take(as.length - as2.length)

      val call = if (as1.isEmpty)
        code"f(Array(${@@(as2)}): _*)"
      else
        code"f(${@@(as1)}, Array(${@@(as2)}): _*)"

      e.args.replace(call.getLastChild)
  }
}
