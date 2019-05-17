package org.jetbrains.plugins.scala
package lang
package parser
package util

import com.intellij.lang.PsiBuilder
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.tree.TokenSet
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder

import scala.annotation.tailrec

object ParserUtils {

  def eatSeqWildcardNext(builder: PsiBuilder): Boolean = {
    val marker = builder.mark
    if (builder.getTokenType == ScalaTokenTypes.tUNDER) {
      builder.advanceLexer()
      if (builder.getTokenType == ScalaTokenTypes.tIDENTIFIER &&
        builder.getTokenText == "*") {
        builder.advanceLexer()
        marker.done(ScalaElementType.SEQ_WILDCARD)
        true
      } else {
        marker.rollbackTo()
        false
      }
    } else {
      marker.drop()
      false
    }
  }

  def isAssignmentOperator: String => Boolean = {
    case "==" | "!=" | "<=" | ">=" => false
    case "=" => true
    case id => id.head != '=' && id.last == '='
  }


  /** Defines the precedence of an infix operator, according
    * to its first character.
    *
    * @param id          The identifier
    * @param assignments Consider assignment operators have lower priority than other non-special characters
    * @return An integer value. Lower value means higher precedence
    */
  def priority(id: String, assignments: Boolean = false): Int = {
    if (assignments && isAssignmentOperator(id)) {
      return 10
    }
    id.charAt(0) match {
      case '~' | '#' | '@' | '?' | '\\' => 0 //todo: other special characters?
      case '*' | '/' | '%'              => 1
      case '+' | '-'                    => 2
      case ':'                          => 3
      case '=' | '!'                    => 4
      case '<' | '>'                    => 5
      case '&'                          => 6
      case '^'                          => 7
      case '|'                          => 8
      case _                            => 9
    }
  }

  @tailrec
  def parseLoopUntilRBrace(builder: ScalaPsiBuilder, fun: () => Unit, braceReported: Boolean = false) {
    var br = braceReported
    fun()
    builder.getTokenType match {
      case ScalaTokenTypes.tRBRACE =>
        builder.advanceLexer()
        return
      case ScalaTokenTypes.tLBRACE => //to avoid missing '{'
        if (!braceReported) {
          builder error ErrMsg("rbrace.expected")
          br = true
        }
        var balance = 1
        builder.advanceLexer()
        while (balance != 0 && !builder.eof) {
          builder.getTokenType match {
            case ScalaTokenTypes.tRBRACE => balance -= 1
            case ScalaTokenTypes.tLBRACE => balance += 1
            case _ =>
          }
          builder.advanceLexer()
        }
        if (builder.eof)
          return
      case _ =>
        if (!braceReported) {
          builder error ErrMsg("rbrace.expected")
          br = true
        }
        builder.advanceLexer()
        if (builder.eof) {
          return
        }
    }
    parseLoopUntilRBrace(builder, fun, br)
  }

  def parseBalancedParenthesis(builder: ScalaPsiBuilder, accepted: TokenSet, count: Int = 1): Boolean = {
    var seen = 0

    builder.getTokenType match {
      case ScalaTokenTypes.tLPARENTHESIS =>
        var count = 1
        builder.advanceLexer()

        while (count > 0 && !builder.eof()) {
          builder.getTokenType match {
            case ScalaTokenTypes.tLPARENTHESIS => count += 1
            case ScalaTokenTypes.tRPARENTHESIS => count -= 1
            case acc if accepted.contains(acc) => seen += 1
            case o => return false
          }

          builder.advanceLexer()
        }
      case _ =>
    }

    seen == count
  }

  def hasTextBefore(builder: ScalaPsiBuilder, text: String): Boolean = {
    Option(builder.getLatestDoneMarker).exists {
      marker =>
        StringUtil.equals(builder.getOriginalText.subSequence(marker.getStartOffset, marker.getEndOffset - 1), text)
    }
  }

  def isBackticked(name: String): Boolean = name != "`" && name.startsWith("`") && name.endsWith("`")

  def isCurrentVarId(builder: PsiBuilder): Boolean = {
    val txt = builder.getTokenText
    !txt.isEmpty && Character.isUpperCase(txt.charAt(0)) || isBackticked(txt)
  }

  def parseVarIdWithWildcardBinding(builder: PsiBuilder, rollbackMarker: PsiBuilder.Marker): Boolean = {
    if (!ParserUtils.isCurrentVarId(builder)) builder.advanceLexer() else {
      rollbackMarker.rollbackTo()
      return false
    }

    builder.advanceLexer() // @
    if (eatSeqWildcardNext(builder)) {
      rollbackMarker.done(ScalaElementType.NAMING_PATTERN)
      true
    } else {
      rollbackMarker.rollbackTo()
      false
    }
  }
}
