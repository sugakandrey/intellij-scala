package org.jetbrains.plugins.scala.lang.psi.impl.base.types

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.dotty.lang.core.Constant
import org.jetbrains.plugins.dotty.lang.core.types.DotConstantType
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.lang.psi.annotator.ScLiteralTypeElementAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScLiteralTypeElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementImpl
import org.jetbrains.plugins.scala.lang.psi.types.ScLiteralType
import org.jetbrains.plugins.scala.lang.psi.types.result.{Failure, ScalaTypeResult, TypeResult}
import org.jetbrains.plugins.scala.lang.typeInference.ConstantTag

class ScLiteralTypeElementImpl(val node: ASTNode) extends ScalaPsiElementImpl(node)
  with ScLiteralTypeElement with ScLiteralTypeElementAnnotator {

  override protected def innerType: TypeResult =
    ConstantTag.fromAstNode(getLiteralNode) match {
      case Some(kind) => Right(ScLiteralType(getLiteral.getValue, kind).blockWiden())
      case _ => Failure(ScalaBundle.message("wrong.psi.for.literal.type", getText))
    }

  override def tpe: ScalaTypeResult = {
    val maybeTag = ConstantTag.fromAstNode(getLiteralNode)

    maybeTag match {
      case Some(tag) =>
        val litTpe =
          if (typeSystem.isDotty) DotConstantType(Constant(getLiteral.getValue, tag))
          else                    ScLiteralType(getLiteral.getValue, tag).blockWiden()
        Right(litTpe)
      case None => Failure(s"Can't get constant tag for $getLiteralNode.")
    }
  }

  override def getLiteral: ScLiteral = getFirstChild.asInstanceOf[ScLiteral]
}
