package org.jetbrains.plugins.scala.lang.psi.types

import com.intellij.openapi.project.Project
import org.apache.commons.lang.StringEscapeUtils
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticFunction
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.{Singleton, TypeVisitor, ValueType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.AfterUpdate.{ProcessSubtypes, ReplaceWith, Stop}
import org.jetbrains.plugins.scala.lang.psi.util.LiteralEvaluationUtil
import org.jetbrains.plugins.scala.lang.typeInference.ConstantTag
import org.jetbrains.plugins.scala.project.ProjectContext

import scala.reflect.ClassTag

class ScLiteralType private(val literalValue: Any, val kind: ConstantTag, val allowWiden: Boolean = true)
                           (implicit val projectContext: ProjectContext) extends ValueType with LeafType {

  override def visitType(visitor: TypeVisitor): Unit = visitor.visitLiteralType(this)

  def wideType: ScType = ScLiteralType.wideType(kind)

  def blockWiden(): ScLiteralType = ScLiteralType.blockWiden(this)

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: ScLiteralType => kind == other.kind && literalValue == other.literalValue
      case _ => false
    }
  }

  override def hashCode(): Int = 31 * kind.hashCode() + literalValue.##

  private def valueAs[T: ClassTag]: T = literalValue.asInstanceOf[T]

  private def valueFromKind: Any =
    kind match {
      case ConstantTag.String  => valueAs[String]
      case ConstantTag.Symbol  => valueAs[Symbol]
      case ConstantTag.Boolean => valueAs[Boolean]
      case ConstantTag.Int     => valueAs[Int]
      case ConstantTag.Long    => valueAs[Long]
      case ConstantTag.Float   => valueAs[Float]
      case ConstantTag.Double  => valueAs[Double]
      case ConstantTag.Char    => valueAs[Char]
    }
}

object ScLiteralType {

  import LiteralEvaluationUtil._

  def apply(literalValue: Any, kind: ConstantTag)(implicit projectContext: ProjectContext): ScLiteralType =
    new ScLiteralType(literalValue, kind)

  private def blockWiden(lit: ScLiteralType): ScLiteralType =
    new ScLiteralType(lit.literalValue, lit.kind, false)(lit.projectContext)

  def wideType(kind: ConstantTag)(implicit projectContext: ProjectContext): ScType = {
    def getCachedClass(fqn: String) =
      ElementScope(projectContext).getCachedClass(fqn)
        .map(ScType.designator).getOrElse(api.Nothing)

    kind match {
      case ConstantTag.Boolean => api.Boolean
      case ConstantTag.String => getCachedClass("java.lang.String")
      case ConstantTag.Symbol => getCachedClass("scala.Symbol")
      case ConstantTag.Int => api.Int
      case ConstantTag.Long => api.Long
      case ConstantTag.Float => api.Float
      case ConstantTag.Double => api.Double
      case ConstantTag.Char => api.Char
    }
  }

  def printValue(lt: ScLiteralType): String = lt.kind match {
    case ConstantTag.String => quoted(lt.valueAs[String])
    case ConstantTag.Char => s"\'${lt.literalValue}\'"
    case ConstantTag.Long => lt.literalValue.toString + "L"
    case ConstantTag.Float => lt.literalValue.toString + "f"
    case ConstantTag.Boolean |
         ConstantTag.Int |
         ConstantTag.Symbol |
         ConstantTag.Double => lt.literalValue.toString
  }

  private def quoted(s: String): String = "\"" + StringEscapeUtils.escapeJava(s) + "\""

  private def isInteger(kind: ConstantTag) = kind match {
    case ConstantTag.Int | ConstantTag.Long | ConstantTag.Char => true
    case _ => false
  }

  def isNumeric(kind: ConstantTag): Boolean =
    isInteger(kind) || kind == ConstantTag.Float || kind == ConstantTag.Double

  def widenRecursive(aType: ScType): ScType = {

    def isSingleton(param: ScTypeParam) = param.upperBound.exists(_.conforms(Singleton(param.projectContext)))

    def widenRecursiveInner(aType: ScType, visited: Set[ScParameterizedType]): ScType = aType.recursiveUpdate {
      case lit: ScLiteralType => ReplaceWith(lit.widen)
      case p: ScParameterizedType if visited(p) => Stop
      case p: ScParameterizedType =>
        p.designator match {
          case ScDesignatorType(des) => des match {
            case typeDef: ScTypeDefinition =>
              val newDesignator = widenRecursiveInner(p.designator, visited + p)
              val newArgs = (typeDef.typeParameters zip p.typeArguments).map {
                case (param, arg) if isSingleton(param) => arg
                case (_, arg) => widenRecursiveInner(arg, visited + p)
              }
              val newDes = ScParameterizedType(newDesignator, newArgs)
              ReplaceWith(newDes)
            case _ => Stop
          }
          case _ => Stop
        }
      case _: ScCompoundType => Stop
      case _ => ProcessSubtypes
    }

    widenRecursiveInner(aType, Set.empty)
  }

  //TODO we have also support for Byte and Short, but that's not a big deal since literal types for them currently can't be parsed
  def foldUnOpTypes(arg: ScLiteralType, fun: ScSyntheticFunction): Option[ScLiteralType] = {
    implicit val projectContext: ProjectContext = arg.projectContext

    val kind = arg.kind
    val name = fun.name
    if (isNumeric(kind)) {
      if (name == "unary_+") Some(arg)
      else if (name == "unary_-") {
        kind match {
          case ConstantTag.Int => Some(ScLiteralType(-arg.valueAs[Int], kind))
          case ConstantTag.Long => Some(ScLiteralType(-arg.valueAs[Long], kind))
          case ConstantTag.Float => Some(ScLiteralType(-arg.valueAs[Float], kind))
          case ConstantTag.Double => Some(ScLiteralType(-arg.valueAs[Double], kind))
          case ConstantTag.Char => Some(ScLiteralType(-arg.valueAs[Char], kind))
          case _ => None
        }
      } else None
    } else None
  }

  def foldBinOpTypes(left: ScLiteralType, right: ScLiteralType, fun: ScSyntheticFunction): Option[ScLiteralType] = {
    val name = fun.name
    implicit val project: Project = fun.getProject
    val res = evaluateConstInfix[ScLiteralType](
      left.valueFromKind,
      right.valueFromKind,
      allowToStringConversion = false,
      name,
      value => ConstantTag.fromValue(value).map(ScLiteralType(value, _))
    )
    res
  }
}