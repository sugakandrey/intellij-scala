package org.jetbrains.plugins.scala.lang.typeInference

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType

sealed trait ConstantTag

object ConstantTag {
  case object Symbol  extends ConstantTag
  case object Unit    extends ConstantTag
  case object Boolean extends ConstantTag
  case object Byte    extends ConstantTag
  case object Short   extends ConstantTag
  case object Char    extends ConstantTag
  case object Int     extends ConstantTag
  case object Long    extends ConstantTag
  case object Float   extends ConstantTag
  case object Double  extends ConstantTag
  case object String  extends ConstantTag
  case object Null    extends ConstantTag
  case object Class   extends ConstantTag

  def fromValue(literalValue: Any): Option[ConstantTag] = literalValue match {
    case _: Unit      => Unit.toOption
    case _: Boolean   => Boolean.toOption
    case _: Char      => Char.toOption
    case _: Byte      => Byte.toOption
    case _: Short     => Short.toOption
    case _: Int       => Int.toOption
    case _: Long      => Long.toOption
    case _: Float     => Float.toOption
    case _: Double    => Double.toOption
    case _: String    => String.toOption
    case _: Symbol    => Symbol.toOption
    case _: String    => String.toOption
    case _: ScalaType => Class.toOption
    case null         => Null.toOption
    case _            => None
  }

  @annotation.tailrec
  def fromAstNode(node: ASTNode): Option[ConstantTag] = {
    def endsWith(c1: Char, c2: Char) = {
      val lastChar = node.getText.lastOption
      lastChar.contains(c1) || lastChar.contains(c2)
    }

    node.getElementType match {
      case ScalaTokenTypes.tINTEGER => (if (endsWith('l', 'L')) Long else Int).toOption
      case ScalaTokenTypes.tFLOAT   => (if (endsWith('f', 'F')) Float else Double).toOption
      case ScalaTokenTypes.tCHAR    => Char.toOption
      case ScalaTokenTypes.tSYMBOL  => Symbol.toOption
      case ScalaTokenTypes.tSTRING | ScalaTokenTypes.tWRONG_STRING |
          ScalaTokenTypes.tMULTILINE_STRING =>
        String.toOption
      case ScalaTokenTypes.kTRUE | ScalaTokenTypes.kFALSE     => Boolean.toOption
      case ScalaTokenTypes.tIDENTIFIER if node.getText == "-" => fromAstNode(node.getTreeNext)
      case _                                                  => None
    }
  }

}
