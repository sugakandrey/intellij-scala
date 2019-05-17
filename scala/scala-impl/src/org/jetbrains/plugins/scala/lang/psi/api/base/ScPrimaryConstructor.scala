package org.jetbrains.plugins.scala
package lang
package psi
package api
package base

import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.light.ScPrimaryConstructorWrapper
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeParameterType
import org.jetbrains.plugins.scala.lang.psi.types.api.designator._
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScMethodType
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.macroAnnotations.{Cached, CachedInUserData, ModCount}

import scala.collection.mutable

/**
* @author Alexander Podkhalyuzin
* Date: 07.03.2008
*/

trait ScPrimaryConstructor extends ScMember with ScMethodLike {

  /**
   *  @return has access modifier
   */
  def hasModifier: Boolean

  def getClassNameText: String

  def parameterList: ScParameters

  def parameters : Seq[ScClassParameter] = parameterList.clauses.flatMap(_.unsafeClassParameters)

  override def containingClass: ScTypeDefinition = getParent.asInstanceOf[ScTypeDefinition]

  /**
   * return only parameters, which are additionally members.
   */
  def valueParameters: Seq[ScClassParameter] = parameters.filter((p: ScClassParameter) => p.isVal || p.isVar)

  /**
   * All classes must have one non-implicit parameter list. If this is not declared in in the code,
   * it is assumed by the compiler.
   *
   * In addition, view and context bounds generate an additional implicit parameter section.
   */
  @CachedInUserData(this, ModCount.getBlockModificationCount)
  def effectiveParameterClauses: Seq[ScParameterClause] = {
    def emptyParameterList: ScParameterClause =
      ScalaPsiElementFactory.createEmptyClassParamClauseWithContext(parameterList)

    val clausesWithInitialEmpty = parameterList.clauses match {
      case Seq() => Seq(emptyParameterList)
      case Seq(clause) if clause.isImplicit => Seq(emptyParameterList, clause)
      case clauses => clauses
    }

    clausesWithInitialEmpty ++
      ScalaPsiUtil.syntheticParamClause(containingClass, parameterList, isClassParameter = true)()
  }

  def effectiveFirstParameterSection: Seq[ScClassParameter] = effectiveParameterClauses.head.unsafeClassParameters

  @Cached(ModCount.getBlockModificationCount, this)
  def getFunctionWrappers: Seq[ScPrimaryConstructorWrapper] = {
    val buffer = mutable.ArrayBuffer.empty[ScPrimaryConstructorWrapper]
    buffer += new ScPrimaryConstructorWrapper(this)
    for {
      first <- parameterList.clauses.headOption
      if first.hasRepeatedParam
      if hasAnnotation("scala.annotation.varargs")
    } {
      buffer += new ScPrimaryConstructorWrapper(this, isJavaVarargs = true)
    }
    buffer
  }
}

object ScPrimaryConstructor {
  object ofClass {
    def unapply(pc: ScPrimaryConstructor): Option[ScClass] = {
      pc.containingClass match {
        case c: ScClass => Some(c)
        case _ => None
      }
    }
  }
}