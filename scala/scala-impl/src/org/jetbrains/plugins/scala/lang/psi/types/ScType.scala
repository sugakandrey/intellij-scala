package org.jetbrains.plugins.scala
package lang
package psi
package types

import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi.PsiNamedElement
import org.jetbrains.plugins.scala.extensions.ifReadAllowed
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAliasDeclaration, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScEarlyDefinitions
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{TypeVisitor, ValueType}
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.project.ProjectContextOwner

import scala.language.implicitConversions

trait ScType extends ScalaType with ProjectContextOwner {
  override type BaseTpe = ScType

  override implicit def typeSystem: ScalaTypeSystem = ScalaTypeSystem.instance

  private var aliasType: Option[AliasType] = _

  final def isAliasType: Option[AliasType] = {
    if (aliasType == null) {
      ProgressManager.checkCanceled()
      aliasType = isAliasTypeInner
    }
    aliasType
  }

  private var unpacked: ScType = _

  final def unpackedType: ScType = {
    if (unpacked == null) {
      ProgressManager.checkCanceled()
      unpacked = unpackedTypeInner
    }
    unpacked
  }

  protected def isAliasTypeInner: Option[AliasType] = None

  override final def toString: String = ifReadAllowed(this.presentableText)(getClass.getSimpleName)

  def isValue: Boolean

  def isFinalType: Boolean = false

  def inferValueType: ValueType

  protected def unpackedTypeInner: ScType = ScExistentialType(this) match {
    case ScExistentialType(q, Seq())                                       => q
    case ScExistentialType(arg: ScExistentialArgument, Seq(w)) if w == arg => arg.upper
    case ex                                                                => ex
  }

  def equivInner(r: ScType, constraints: ConstraintSystem, falseUndef: Boolean): ConstraintsResult =
    ConstraintsResult.Left

  def visitType(visitor: TypeVisitor)

  def typeDepth: Int = 1
}

object ScType {
  implicit def recursiveUpdateExtension(tpe: ScType): recursiveUpdate.Extensions[ScType] =
    new recursiveUpdate.Extensions[ScType](tpe)
  /**
    * Expands type aliases, including those in a type projection. Type Alias Declarations are replaced by their upper
    * bound.
    *
    * @see https://youtrack.jetbrains.net/issue/SCL-2872
    */
  // TODO This is all a bit ad-hoc. What can we learn from scalac?
  // TODO perhaps we need to choose the lower bound if we are in a contravariant position. We get away
  //      with this as we currently only rely on this method to determine covariant types: the parameter
  //      types of FunctionN, or the elements of TupleN
  def expandAliases(tp: ScType, visited: Set[ScType] = Set.empty): TypeResult = {

    if (visited contains tp) return Right(tp)
    tp match {
      case proj@ScProjectionType(_, _) => proj.actualElement match {
        case t: ScTypeAliasDefinition if t.typeParameters.isEmpty =>
          t.aliasedType.flatMap(t => expandAliases(proj.actualSubst(t), visited + tp))
        case t: ScTypeAliasDeclaration if t.typeParameters.isEmpty =>
          t.upperBound.flatMap(upper => expandAliases(proj.actualSubst(upper), visited + tp))
        case _ => Right(tp)
      }
      case at: ScAbstractType => expandAliases(at.upper, visited + tp) // ugly hack for SCL-3592
      case ScDesignatorType(t: ScType) => expandAliases(t, visited + tp)
      case ScDesignatorType(ta: ScTypeAliasDefinition) => expandAliases(ta.aliasedType.getOrNothing, visited + tp)
      case t: ScTypeAliasDeclaration if t.typeParameters.isEmpty =>
        t.upperBound.flatMap(expandAliases(_, visited + tp))
      case t: ScTypeAliasDefinition if t.typeParameters.isEmpty =>
        t.aliasedType
      case pt: ScParameterizedType if pt.isAliasType.isDefined =>
        val aliasType: AliasType = pt.isAliasType.get
        aliasType.upper.flatMap(expandAliases(_, visited + tp))
      case _ => Right(tp)
    }
  }

  /**
    * Creates a type that designates `element`. Usually this will be a ScDesignatorType, except for the
    * special case when `element` represent a standard type, such as scala.Double.
    *
    * @see https://youtrack.jetbrains.net/issue/SCL-2913
    */
  def designator(element: PsiNamedElement): ScType = {
    element match {
      case clazz: ScClass if !Option(clazz.getContext).exists(c => c.isInstanceOf[ScTemplateBody] || c.isInstanceOf[ScEarlyDefinitions]) =>
        val designatorType = ScDesignatorType(element)
        designatorType.getValType.getOrElse(designatorType)
      case _ =>
        val clazzOpt = element match {
          case p: ScClassParameter => Option(p.containingClass)
          case _ => element.getContext match {
            case _: ScTemplateBody | _: ScEarlyDefinitions =>
              Option(ScalaPsiUtil.contextOfType(element, strict = true, classOf[ScTypeDefinition]))
            case _ => None
          }
        }

        clazzOpt match {
          case Some(clazz) => ScProjectionType(ScThisType(clazz), element)
          case _ => ScDesignatorType(element)
        }
    }
  }
}

trait NamedType extends ScType {
  val name: String
}
