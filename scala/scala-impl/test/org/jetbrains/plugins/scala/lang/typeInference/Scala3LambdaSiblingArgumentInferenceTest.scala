package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** Scala 3 types an untyped function literal after the other arguments of its clause fixed the type parameters. */
class Scala3LambdaSiblingArgumentInferenceTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  private val defs =
    """trait Eq[A]
      |object Eq:
      |  given Eq[String] = ???
      |trait Callback
      |object Callback:
      |  def empty: Callback = ???
      |object Bare:
      |  enum Sel:
      |    case A, B
      |  object Sel:
      |    given Eq[Sel] = ???
      |  final case class St(sel: Sel, n: Int)
      |  def d10[T](selected: T, onSelect: T => Int): Int = 0
      |  def two[T](selected: T, onChange: (T, Int) => Int): Int = 0
      |  def options[T](options: List[(String, T)], onSelect: T => Int): Int = 0
      |  def group[T: Eq](label: String, options: List[(String, T)], selected: T, onSelect: T => Callback,
      |                   enabled: Boolean = true, hint: String = ""): Int = 0
      |""".stripMargin

  def testLiteralBody(): Unit = checkTextHasNoErrors(defs + "  val s = d10(Sel.A, v => 1)\n")

  def testIdentityBody(): Unit = checkTextHasNoErrors(defs + "  val s = d10(1, v => v + 1)\n")

  def testTwoParameters(): Unit = checkTextHasNoErrors(defs + "  val s = two(Sel.A, (v, i) => i)\n")

  def testTypeParameterInsideTuple(): Unit = checkTextHasNoErrors(defs + "  val s = options(List(\"a\" -> Sel.A), v => 1)\n")

  def testBracesLambda(): Unit = checkTextHasNoErrors(defs + "  val s = d10(Sel.A, { v => 1 })\n")

  def testNamedLambdaArgument(): Unit = checkTextHasNoErrors(defs + "  val s = d10(onSelect = v => 1, selected = Sel.A)\n")

  def testParameterUsedInNamedCopyArgument(): Unit = checkTextHasNoErrors(
    defs +
      """  def render(state: St, setState: St => Callback): Int =
        |    group("View", List("A" -> Sel.A, "B" -> Sel.B), state.sel, v => setState(state.copy(sel = v)))
        |""".stripMargin
  )

  def testWithinTypedContext(): Unit = checkTextHasNoErrors(defs + "  def f(x: Sel): Int = d10(x, v => 1)\n")

  def testUnderscoreSectionSiblingIsUntypedToo(): Unit = checkTextHasNoErrors(
    """def decode[A](e: Either[String, A]): Either[Throwable, A] =
      |  e.fold(s => Left(new RuntimeException(s)), Right(_))
      |def decodeFlipped[A](e: Either[Throwable, A]): Either[Throwable, A] =
      |  e.fold(Left(_), a => Right(a))
      |""".stripMargin
  )
}
