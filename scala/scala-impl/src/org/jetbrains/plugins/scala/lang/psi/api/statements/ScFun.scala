package org.jetbrains.plugins.scala.lang.psi.api.statements

import scala.collection.Seq
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.typeInference.Parameter

//some functions are not PsiMethods and are e.g. not visible from java
//see ScSyntheticFunction
trait ScFun extends ScTypeParametersOwner {
  def retType: ScType

  def paramClauses: Seq[Seq[Parameter]]
}
