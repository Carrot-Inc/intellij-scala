package org.jetbrains.plugins.scala.lang.macros.evaluator.impl

import org.jetbrains.plugins.scala.extensions.ObjectExt
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.api.{FunctionType, ParameterizedType}
import org.jetbrains.plugins.scala.lang.psi.ElementScope
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, api}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.lang.psi.types._

import scala.annotation.tailrec
import scala.util.Try

/**
 * Types `Focus[S](_.a.b)`, `GenLens[S](_.a.b)` and `Focus[S](s => s.a.b)` as `monocle.Lens[S, B]`.
 *
 * `monocle.Focus.MkFocus.apply` is a `transparent inline` macro whose result type is only known after expansion,
 * and whose parameter is a context function the plugin cannot apply a lambda to. Without this the call is
 * mistyped, every use of the lens falls back to an implicit search, and the failed resolution is repeated on every
 * re-typing of the enclosing expression.
 *
 * Only plain field paths are handled; the Focus DSL (`each`, `some`, `at`, `withDefault`, …) is left to the
 * regular typing.
 */
object MonocleFocusApply {
  private val MkFocusName = "MkFocus"
  private val MkFocusQualifiedName = "monocle.Focus.MkFocus"

  def typeOf(invokedType: ScType, invocation: MethodInvocation): Option[ScType] =
    invocation.argumentExpressions match {
      case Seq(argument) =>
        implicit val context: Context = Context(invocation)
        for {
          from      <- mkFocusTypeArgument(invokedType)
          path      <- fieldPath(argument)
          fieldType <- typeOfPath(from, path, invocation)
          lens      <- ScalaPsiElementFactory.createTypeFromText(
            s"_root_.monocle.Lens[${from.canonicalText}, ${fieldType.canonicalText}]",
            invocation.getContext,
            invocation
          )
        } yield lens
      case _ => None
    }

  /**
   * Expected type of the path argument, `S => Any`, so that the placeholder or lambda parameter is typed as `S` and
   * the path resolves, even though the plugin cannot derive it from the context-function parameter itself.
   *
   * The receiver is recognised syntactically (`Focus[S]` / `GenLens[S]` resolving to the monocle factory) rather
   * than by typing the generic call: its type is computed with this invocation's argument clause attached, which
   * would re-enter the typing that asked for the expected type in the first place.
   */
  def expectedArgumentType(invocation: MethodInvocation, argument: ScExpression): Option[ScType] =
    invocation.getEffectiveInvokedExpr match {
      case generic: ScGenericCall if invocation.argumentExpressions == Seq(argument) && isFocusFactory(generic.referencedExpr) =>
        implicit val context: Context = Context(invocation)
        implicit val projectContext: ProjectContext = invocation.projectContext
        implicit val scope: ElementScope = invocation.elementScope
        for {
          _    <- fieldPath(argument)
          from <- focusTypeArgument(generic)
        } yield FunctionType(api.Any, Seq(from))
      case _ => None
    }

  private val FocusFactories = Set("monocle.Focus", "monocle.macros.GenLens")

  private def isFocusFactory(expression: ScExpression): Boolean = expression match {
    case reference: ScReferenceExpression =>
      reference.resolve() match {
        case obj: ScObject     => FocusFactories(obj.qualifiedName)
        case fun: ScFunction   => fun.name == "apply" && Option(fun.containingClass).exists(c => FocusFactories(c.qualifiedName))
        case _                 => false
      }
    case _ => false
  }

  private def focusTypeArgument(generic: ScGenericCall): Option[ScType] =
    generic.typeArgs.typeArguments match {
      case Seq(single) => single.typeElement.flatMap(_.`type`().toOption)
      case _           => None
    }

  private def mkFocusTypeArgument(invokedType: ScType)(implicit context: Context): Option[ScType] =
    invokedType match {
      case ParameterizedType(designator, Seq(from)) =>
        designator.extractClass.collect {
          case definition: ScTypeDefinition
            if definition.name == MkFocusName && definition.qualifiedName == MkFocusQualifiedName => from
        }
      case _ => None
    }

  private def fieldPath(argument: ScExpression): Option[List[String]] = argument match {
    case ScParenthesisedExpr(inner)  => fieldPath(inner)
    case reference: ScReferenceExpression =>
      pathFromRoot(reference, _.is[ScUnderscoreSection], Nil)
    case function: ScFunctionExpr =>
      (function.parameters, function.result) match {
        case (Seq(parameter), Some(body: ScReferenceExpression)) =>
          val isParameter: ScExpression => Boolean = {
            case root: ScReferenceExpression => root.qualifier.isEmpty && root.refName == parameter.name
            case _                           => false
          }
          pathFromRoot(body, isParameter, Nil)
        case _ => None
      }
    case _ => None
  }

  @tailrec
  private def pathFromRoot(
    reference: ScReferenceExpression,
    isRoot:    ScExpression => Boolean,
    suffix:    List[String]
  ): Option[List[String]] =
    reference.qualifier match {
      case Some(qualifier) if isRoot(qualifier)               => Some(reference.refName :: suffix)
      case Some(qualifier: ScReferenceExpression)             => pathFromRoot(qualifier, isRoot, reference.refName :: suffix)
      case _                                                   => None
    }

  private def typeOfPath(from: ScType, path: List[String], invocation: MethodInvocation)
                        (implicit context: Context): Option[ScType] = {
    val text = s"((focusRoot: ${from.canonicalText}) => focusRoot.${path.mkString(".")})"
    Try(ScalaPsiElementFactory.createExpressionWithContextFromText(text, invocation.getContext, invocation))
      .toOption
      .flatMap(_.`type`().toOption)
      .collect { case FunctionType(result, _) => result }
  }
}
