package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** Scala 3 keeps a method's type parameters open until its last explicit parameter clause is applied
 * (`interpolateTypeVars` waits while the tree's type is still a method type), so a later clause can widen
 * what an earlier one inferred. Scala 2 instantiates them after the first clause.
 */
class Scala3MultiClauseTypeParameterInferenceTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  private val zio =
    """trait Z[-R, +E, +A]
      |object Z:
      |  def unit: Z[Any, Nothing, Unit] = ???
      |  def succeed[A](a: => A): Z[Any, Nothing, A] = ???
      |trait HGS
      |""".stripMargin

  def testFoldSeedWidenedByLambda(): Unit = checkTextHasNoErrors(
    zio +
      """def eff(c: String): Z[HGS, Throwable, Unit] = ???
        |def f(opt: Option[String]): Z[HGS, Throwable, Unit] = opt.fold(Z.unit) { c => eff(c) }
        |def g(opt: Option[String]): Z[HGS, Throwable, Unit] = opt.fold(Z.unit)(c => eff(c))
        |""".stripMargin
  )

  def testFoldSeedWidenedByMethodReference(): Unit = checkTextHasNoErrors(
    zio +
      """trait Migration
        |def cancel(m: Migration): Z[HGS, Throwable, Either[String, Unit]] = ???
        |def f(running: Option[Migration]): Z[HGS, Throwable, Either[String, Unit]] =
        |  running.fold(Z.succeed(Left("no running migration")))(cancel)
        |""".stripMargin
  )

  def testFoldEmptyListSeedWidenedByLambda(): Unit = checkTextHasNoErrors(
    """def images(byType: Map[String, List[Int]], key: Option[String]): List[Int] =
      |  byType.get("k").fold(List.empty) { xs => key.map(_ => xs).getOrElse(List.empty) }
      |""".stripMargin
  )

  def testSecondClauseWidensFirst(): Unit = checkTextHasNoErrors(
    """def two[A](x: A)(y: A): A = x
      |val t = two(1)("s")
      |val u: Int = two(1)(2)
      |""".stripMargin
  )

  def testFoldLeftNilSeed(): Unit = checkTextHasNoErrors(
    """val l: List[Int] = List(1, 2).foldLeft(Nil)((acc, x) => x :: acc)
      |""".stripMargin
  )

  def testLambdaParameterStillTypedFromFirstClause(): Unit = checkTextHasNoErrors(
    """def withIt[A](a: A)(f: A => Int): Int = f(a)
      |val n: Int = withIt(1)(x => x + 1)
      |val s: Int = withIt("a")(x => x.length)
      |""".stripMargin
  )

  def testImplicitClauseStillResolvedFromFirstClause(): Unit = checkTextHasNoErrors(
    """trait Show[A]
      |given Show[Int] = ???
      |def show[A](a: A)(using Show[A]): String = ???
      |val s: String = show(1)
      |def showThen[A](a: A)(using Show[A])(f: A => String): String = f(a)
      |val t: String = showThen(1)(x => (x + 1).toString)
      |""".stripMargin
  )
}
