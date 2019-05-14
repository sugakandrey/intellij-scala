package org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate

import org.jetbrains.plugins.scala.lang.psi.types.ScalaType
import org.jetbrains.plugins.scala.lang.psi.types.api.Variance
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate._

trait Update[Tpe <: ScalaType] extends ((Tpe, Variance) => AfterUpdate[Tpe])

  object Update {
    import AfterUpdate._

    def apply[Tpe <: ScalaType](pf: PartialFunction[(Tpe, Variance), Tpe]): Update[Tpe] =
      (v1: Tpe, v2: Variance) => {
        if (pf.isDefinedAt(v1, v2)) ReplaceWith(pf(v1, v2))
        else ProcessSubtypes
      }
  }

  trait SimpleUpdate[Tpe <: ScalaType] extends (Tpe => AfterUpdate[Tpe]) with Update[Tpe] {
    def apply(v1: Tpe, v2: Variance): AfterUpdate[Tpe] = apply(v1)
  }

  object SimpleUpdate {
    def apply[Tpe <: ScalaType](pf: PartialFunction[Tpe, Tpe]): SimpleUpdate[Tpe] = (tp: Tpe) => {
      if (pf.isDefinedAt(tp)) ReplaceWith(pf(tp))
      else ProcessSubtypes
    }
  }
