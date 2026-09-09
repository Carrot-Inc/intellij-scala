package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** An expected type that contradicts a type parameter's own bounds must not drive inference in Scala 3
 * (`constrainResult` forgets it); the arguments decide and the result is adapted by a view afterwards.
 */
class Scala3ExpectedTypeContradictingBoundsTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  def testGetOrElseAdaptedToExpectedTypeByConversion(): Unit = checkTextHasNoErrors(
    """trait VdomNode
      |given Conversion[String, VdomNode] = ???
      |def node(n: VdomNode): Int = 0
      |def f(o: Option[String]): Int = node(o.getOrElse("x"))
      |val n: VdomNode = Option("a").getOrElse("x")
      |def g(o: Option[String]): String = o.getOrElse("x")
      |""".stripMargin
  )

  def testGetOrElseAsNamedArgumentOfCaseClassApply(): Unit = checkTextHasNoErrors(
    """trait VdomNode
      |given Conversion[String, VdomNode] = ???
      |final case class Item(title: VdomNode, text: String)
      |def item(print: Option[String]): Item = Item(title = print.getOrElse("Select"), text = "")
      |""".stripMargin
  )

  def testGetOrElseInsideInterpolatorExpectingShown(): Unit = checkTextHasNoErrors(
    """trait Show[A]
      |final case class Shown(s: String)
      |object Shown:
      |  given shownFrom[A: Show]: Conversion[A, Shown] = ???
      |given Show[String] = ???
      |extension (sc: StringContext) def show(args: Shown*): String = ???
      |def line(license: Option[String]): String = show"(${license.getOrElse("N/A")})"
      |""".stripMargin
  )

  def testNoConversionStillReportsMismatch(): Unit = checkHasErrorAroundCaret(
    """trait VdomNode
      |def f(o: Option[String]): VdomNode = o.getOr<caret>Else("x")
      |""".stripMargin
  )
}
