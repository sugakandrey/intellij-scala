package org.jetbrains.plugins.scala.lang.psi.api.base.types

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.types.result.Failure

/**
  * Represents Scala 3 type lambdas of shape `[+X >: L <: H, -Y] => F[X]`.
  * Note: only the upper bound (`H` in the example above) can be F-bounded.
  */
trait ScTypeLambdaTypeElement extends ScTypeElement with ScTypeParametersOwner {
  override private[types] def getType = Failure("Type lambdas are not supported in Scala 2.")

  def resultTypeElement: ScTypeElement
}
