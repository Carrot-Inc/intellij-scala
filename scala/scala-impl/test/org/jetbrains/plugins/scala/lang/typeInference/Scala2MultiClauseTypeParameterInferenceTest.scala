package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** Scala 2 instantiates a method's type parameters after the first parameter clause. */
class Scala2MultiClauseTypeParameterInferenceTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_2_13)

  def testSecondClauseCannotWidenFirst(): Unit = checkHasErrorAroundCaret(
    """object Test {
      |  def two[A](x: A)(y: A): A = x
      |  val t = two(1)(<caret>"s")
      |}
      |""".stripMargin
  )

  def testFoldSeedFixedByFirstClause(): Unit = checkHasErrorAroundCaret(
    """object Test {
      |  def f(opt: Option[String]): Any = opt.fold(0)(c => <caret>c)
      |}
      |""".stripMargin
  )
}
