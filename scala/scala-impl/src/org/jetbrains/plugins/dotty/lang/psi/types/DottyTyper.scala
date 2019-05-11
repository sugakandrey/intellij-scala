package org.jetbrains.plugins.dotty.lang.psi.types

import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi.PsiModifier
import org.jetbrains.plugins.dotty.lang.core.symbols.TemplateDefSymbol
import org.jetbrains.plugins.dotty.lang.core.types.DottyDefinitions._
import org.jetbrains.plugins.dotty.lang.core.types._
import org.jetbrains.plugins.dotty.lang.core.{Constant, Refinement}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.types._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScTypeBoundsOwner, ScTypeParametersOwner}
import org.jetbrains.plugins.scala.lang.psi.impl.statements.params.ScParameterImpl
import org.jetbrains.plugins.scala.lang.psi.types.api.{TypeSystem, Typer}
import org.jetbrains.plugins.scala.lang.psi.types.result.{DotTypeResult, Typeable, _}
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import org.jetbrains.plugins.scala.lang.typeInference._

trait DottyTyper extends Typer[DotType] { this: TypeSystem[DotType] =>
  private[this] implicit val ts: TypeSystem[DotType] = this

  override def tpe(target: Typeable): TypeResultT[DotType] = notImplemented

  override def extractTypeBound(
    owner:   ScTypeBoundsOwner,
    isLower: Boolean
  ): DotTypeResult = {
    def defaultTpe(params: Seq[DotTypeParameter]): DotType =
      if (isLower) Nothing
      else         HKAny(params)

    val maybeTe =
      if (isLower) owner.lowerTypeElement
      else         owner.upperTypeElement

    val tparams = owner match {
      case tparamOwner: ScTypeParametersOwner => tparamOwner.typeParameters.map(TypeParameter.dot)
      case _                                  => Seq.empty
    }

    maybeTe match {
      case None     => Right(defaultTpe(tparams))
      case Some(te) => typedTypeElement(te).map(DotHKTypeLambda(tparams, _))
    }
  }

  private def notImplemented: DotTypeResult = Failure("Not yet implemented")

  /* ================================= DEFINITIONS =================================*/
  private def typedDefinition(e: ScalaPsiElement): DotTypeResult = e match {
    case alias: ScTypeAlias             => typedTypeAlias(alias)
    case varDef: ScVariableDefinition   => typedVarDef(varDef)
    case varDecl: ScVariableDeclaration => typedVarDecl(varDecl)
    case patDef: ScPatternDefinition    => typedPatDef(patDef)
    case param: ScParameterImpl         => typedParameter(param)
    case tdef: ScTypeDefinition         => typedTypeDef(tdef)
  }

  private def typedTypeDef(tdef: ScTypeDefinition): DotTypeResult =
    Right(TemplateDefSymbol.fromPsi(tdef).typeRef)

  private def typedParameter(param: ScParameterImpl): DotTypeResult = notImplemented

  private def typedPatDef(patDef: ScPatternDefinition): DotTypeResult = patDef.typeElement match {
    case Some(te) => typedTypeElement(te)
    case None =>
      patDef.expr
        .flatMapTypeResult(typedExpression)
        .map {
          case ctp: DotConstantType if patDef.hasModifierProperty(PsiModifier.FINAL) => ctp
          case tpe                                                                   => tpe.widen.widenUnion
        }
  }

  private def typedVarDecl(varDecl: ScVariableDeclaration): DotTypeResult =
    varDecl.typeElement.flatMapTypeResult(typedTypeElement)

  private def typedVarDef(varDef: ScVariableDefinition): DotTypeResult = varDef.typeElement match {
    case Some(te) => typedTypeElement(te)
    case None     => varDef.expr.flatMapTypeResult(typedExpression).map(_.widen.widenUnion)
  }

  private def typedTypeAlias(alias: ScTypeAlias): DotTypeResult = alias match {
    case decl: ScTypeAliasDeclaration =>
      for {
        lo <- extractTypeBound(decl, isLower = true)
        hi <- extractTypeBound(decl, isLower = false)
      } yield DotTypeBounds(lo, hi)
    case defn: ScTypeAliasDefinition => extractTypeBound(defn, isLower = false)
  }

  /* ================================= EXPRESSIONS ================================= */
  private def typedExpression(expr: ScExpression): DotTypeResult = expr match {
    case lit: ScLiteral             => typedLiteral(lit)
    case fun: ScFunctionExpr        => typedFunExpr(fun)
    case block: ScBlock             => typedBlock(block)
    case scIf: ScIf                 => typedIf(scIf)
    case matchExpr: ScMatch         => typedMatch(matchExpr)
    case scFor: ScFor               => typedForComp(scFor)
    case _: ScUnitExpr              => Right(Unit)
    case thisRef: ScThisReference   => typedThisRef(thisRef)
    case superRef: ScSuperReference => typedSuperRef(superRef)
    case _: ScWhile                 => Right(Unit)
    case _: ScDo                    => Right(Unit)
    case typed: ScTypedExpression   => typedTypedExpression(typed)
    case other                      => Failure(s"Can't infer type of ${other.getText}")
  }

  private def typedIf(scIf: ScIf): DotTypeResult = {
    val thenpTpe = scIf.thenExpression.flatMapTypeResult(typedExpression).getOrAny
    val elsepTpe = scIf.elseExpression.flatMapTypeResult(typedExpression).getOrElse(Unit)
    Right(lub(thenpTpe, elsepTpe))
  }

  private def typedBlock(block: ScBlock): DotTypeResult =
    if (block.hasCaseClauses) {
      notImplemented // TODO
    } else
      block.lastExpr match {
        case None       => Right(Unit)
        case Some(expr) => typedExpression(expr)
      }

  private def typedThisRef(thisRef: ScThisReference): DotTypeResult = {
    val maybeTemplate = thisRef.refTemplate

    maybeTemplate match {
      case None       => Failure("Unable to find template definition for `this` reference.")
      case Some(tdef) => Right(DotThisType(TemplateDefSymbol.fromPsi(tdef)))
    }
  }

  private def typedSuperRef(superRef: ScSuperReference): DotTypeResult = {
    val maybeTemplate = superRef.drvTemplate

    maybeTemplate match {
      case None => Failure("Unable to find template definition for `super` reference.")
      case Some(tdef) =>
        val superTpes: Seq[DotType] = ??? // tdef.superTypes
        val superInfo = glb(superTpes, checkWeak = false)
        Right(DotSuperType(TemplateDefSymbol.fromPsi(tdef), superInfo))
    }
  }

  private def typedFunExpr(fun: ScFunctionExpr): DotTypeResult =  {
    val paramTpes = fun.parameters.map(typedDefinition(_).getOrNothing)
    val resultTpe = fun.result.map(typedExpression(_).getOrAny)
    Right(FunctionType(paramTpes, resultTpe.getOrElse(Unit)))
  }

  private def typedForComp(scFor: ScFor): DotTypeResult = notImplemented

  private def typedLiteral(lit: ScLiteral): DotTypeResult =
    ConstantTag.fromAstNode(lit.getFirstChild.getNode) match {
      case Some(tag) => Right(DotConstantType(Constant(lit.getValue, tag)))
      case None      => Failure(s"Unable to get constant tag for $lit.")
    }

  private def typedMatch(mtch: ScMatch): DotTypeResult = {
    val branchTpes = mtch.expressions.map(typedExpression(_).getOrNothing)
    Right(lub(branchTpes, checkWeak = false))
  }

  private def typedTypedExpression(typed: ScTypedExpression): DotTypeResult = {
    lazy val expr = typed.expr

    typed.typeElement match {
      case Some(te)                                     => typedTypeElement(te)
      case _ if !expr.isInstanceOf[ScUnderscoreSection] => typedExpression(expr)
      case _                                            => Failure("Typed expression is not complete for underscore section.")
    }
  }

  /* ===================================== TYPE ELEMENTS ===================================== */
  @annotation.tailrec
  private def typedTypeElement(te: ScTypeElement): DotTypeResult = te match {
    case simple: ScSimpleTypeElement               => typedSimpleType(simple)
    case parameterized: ScParameterizedTypeElement => typedParameterizeType(parameterized)
    case dte: ScDesugarizableTypeElement           => typedDesugarizableType(dte)
    case lambda: ScTypeLambdaTypeElement           => typedTypeLambda(lambda)
    case wc: ScWildcardTypeElement                 => typedWildcardType(wc)
    case proj: ScTypeProjection                    => typedProjectionType(proj)
    case lte: ScLiteralTypeElement                 => typedLiteral(lte.getLiteral)
    case annot: ScAnnotTypeElement                 => typedTypeElement(annot.typeElement)
    case comp: ScCompoundTypeElement               => typedCompoundType(comp)
    case tvar: ScTypeVariableTypeElement           => notImplemented // TODO
    case pte: ScParenthesisedTypeElement           => typedParenthesisedType(pte)
    case _: ScExistentialTypeElement               => Failure("Existential types are not supported in dotty.")
    case unknown                                   => Failure(s"Unknown type element ${unknown.getClass}")
  }

  private def typedSimpleType(ste: ScSimpleTypeElement): DotTypeResult = Failure(s"Can't type SimpleTypeElement yet.")

  private def typedWildcardType(wc: ScWildcardTypeElement): DotTypeResult =
    for {
      lo <- wc.lowerTypeElement.flatMapTypeResult(typedTypeElement)
      hi <- wc.upperTypeElement.flatMapTypeResult(typedTypeElement)
    } yield DotTypeBounds(lo, hi)

  private def traverseTypes[T <: Typeable](
    targs: Seq[T]
  )(f:     T => DotTypeResult = tpe(_)
  ): TypeResultT[List[DotType]] = {
    @annotation.tailrec
    def aux(args: Seq[T], acc: List[DotType] = Nil): TypeResultT[List[DotType]] =
      if (args.isEmpty) Right(acc)
      else {
        ProgressManager.checkCanceled()
        f(args.head) match {
          case Right(tpe)    => aux(args.tail, tpe :: acc)
          case Left(failure) => Left(failure)
        }
      }

    aux(targs).map(_.reverse)
  }

  private def typedParameterizeType(te: ScParameterizedTypeElement): DotTypeResult = {
    def typeArgElements: List[ScTypeElement] = te.typeArgList.typeArgs.toList

    val teTokenType = te.getNode.getElementType
    val isAndTpe    = teTokenType == ScalaTokenTypes.tAND
    val isOrTpe     = teTokenType == ScalaTokenTypes.tOR

    if (isAndTpe || isOrTpe) {
      traverseTypes(typeArgElements)(typedTypeElement).flatMap {
        case lhs :: rhs :: Nil =>
          if (isOrTpe) Right(DotOrType(lhs, rhs))
          else         Right(DotAndType(lhs, rhs))
        case _ => Failure("Wrong number of arguments for &/| type.")
      }
    } else
      for {
        baseTpe <- typedTypeElement(te.typeElement)
        args    <- traverseTypes(typeArgElements)()
      } yield DotAppliedType(baseTpe, args)
  }

  private def typedParenthesisedType(te: ScParenthesisedTypeElement): DotTypeResult =
    te.innerElement match {
      case None          => Right(Unit)
      case Some(innerTe) => typedTypeElement(innerTe)
    }

  private def typedProjectionType(projection: ScTypeProjection): DotTypeResult = {
    projection.bind() match {
      case Some(ScalaResolveResult(elem, _)) =>
        for {
          baseTpe <- typedTypeElement(projection.typeElement)
          symbol  <- TemplateDefSymbol.fromPsi(elem)
        } yield DotTypeRef(baseTpe, symbol)
      case _ => Failure("Cannot resolve projection reference.")
    }
  }

  private def typedCompoundType(compound: ScCompoundTypeElement): DotTypeResult = {
    val parentTpe = compound.components.foldLeft(Any) { case (acc, te) =>
      acc & typedTypeElement(te).getOrAny
    }

    compound.refinement match {
      case Some(scRefinement) =>
        val refinement  = Refinement.fromPsi(scRefinement)
        val refinedType = DotRefinedType(parentTpe, refinement)
        Right(refinedType)
      case None => Right(parentTpe)
    }
  }

  private def typedTypeLambda(lambda: ScTypeLambdaTypeElement): DotTypeResult =  {
    val resTpe  = typedTypeElement(lambda.resultTypeElement)
    val tparams = lambda.typeParameters.map(TypeParameter.dot)
    resTpe.map(DotHKTypeLambda(tparams, _))
  }

  private def typedDesugarizableType(dte: ScDesugarizableTypeElement): DotTypeResult =
    dte.computeDesugarizedType.flatMapTypeResult(typedTypeElement)
}

