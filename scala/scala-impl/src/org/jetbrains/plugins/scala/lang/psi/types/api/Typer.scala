package org.jetbrains.plugins.scala.lang.psi.types.api

import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.Key
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition, ScMacroDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeBoundsOwner
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType
import org.jetbrains.plugins.scala.lang.psi.types.result.{TypeResultT, Typeable}
import org.jetbrains.plugins.scala.project.UserDataHolderExt
import org.jetbrains.plugins.scala.extensions.StubBasedExt
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType._
import org.jetbrains.plugins.scala.lang.psi.impl.statements.ScFunctionImpl
import org.jetbrains.plugins.scala.JavaArrayFactoryUtil.ScFunctionFactory

trait Typer[Tpe <: ScalaType] { this: TypeSystem[Tpe] =>
  import Typer._

  def tpe(target: Typeable): TypeResultT[Tpe]

  /**
    * Given the type element of a type bound returns
    * actual type bound. It may differ from `typeElement.tpe` due to the
    * eta-expansion or simplification of parameterised
    * definition bounds to type lambdas
    * (e.g. `type F[X] <: Foo[X, X]` is in fact `type F <: [X] => Foo[X, X]` in dotty).
    */
  def extractTypeBound(
    owner:   ScTypeBoundsOwner,
    isLower: Boolean
  ): TypeResultT[Tpe]

  /** Widens inferred type before assigning it to a
    * val/def definition.
    */
  protected def widenDefTpe(tpe: Tpe): Tpe

  def returnType(fun: ScFunction): TypeResultT[Tpe] = {
    if (importantOrderFunction(fun)) {
      val parent = fun.getParent
      val isCalculating = isCalculatingFor(parent)

      if (isCalculating.get()) returnTypeInner(fun)
      else {
        isCalculating.set(true)
        try {
          val children = parent.stubOrPsiChildren(FUNCTION_DEFINITION, ScFunctionFactory).iterator

          while (children.hasNext) {
            val nextFun = children.next()
            if (importantOrderFunction(nextFun)) {
              ProgressManager.checkCanceled()
              returnTypeInner(nextFun.asInstanceOf[ScFunctionImpl[_]])
            }
          }
          returnTypeInner(fun)
        }
        finally {
          isCalculating.set(false)
        }
      }
    } else returnTypeInner(fun)
  }

  protected def returnTypeInner(fun: ScFunction): TypeResultT[Tpe] = fun match {
    case decl: ScFunctionDeclaration => decl.returnTypeElement.foldTypeResult(Unit)(tpe)
    case macroDef: ScMacroDefinition => macroDef.returnTypeElement.foldTypeResult(Any)(tpe)
    case defn: ScFunctionDefinition =>
      defn.returnTypeElement match {
        case None if !defn.hasAssign => Right(Unit)
        case Some(te)                => tpe(te)
        case None                    => defn.body.foldTypeResult(Unit)(tpe).map(widenDefTpe)
      }
  }
}

object Typer {
  private[this] val calculatingBlockKey: Key[ThreadLocal[Boolean]] =
    Key.create("calculating.function.returns.block")

  private[Typer] def importantOrderFunction(function: ScFunction): Boolean = function match {
    case funDef: ScFunctionDefinition =>
      funDef.hasModifierProperty("implicit") && !funDef.hasExplicitType
    case _ => false
  }

  private[Typer] def isCalculatingFor(e: PsiElement) = e.getOrUpdateUserData(
    calculatingBlockKey,
    ThreadLocal.withInitial[Boolean](() => false)
  )
}
