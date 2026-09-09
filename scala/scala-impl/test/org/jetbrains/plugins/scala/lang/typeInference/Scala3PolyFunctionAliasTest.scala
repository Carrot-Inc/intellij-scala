package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

class Scala3PolyFunctionAliasTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  def testAliasOfPolymorphicFunctionType(): Unit = checkTextHasNoErrors(
    """trait Task[A]
      |trait SpaceId
      |type RunCTask = [T] => SpaceId => Task[T] => Task[T]
      |def run(runCTask: RunCTask): Int = 0
      |def mk: RunCTask = [T] => (space: SpaceId) => (task: Task[T]) => task
      |def use(r: RunCTask, s: SpaceId, t: Task[Int]): Task[Int] = r[Int](s)(t)
      |def inline(r: [T] => SpaceId => Task[T] => Task[T]): Int = 0
      |val v = run(mk)
      |""".stripMargin
  )

  def testAliasOfPolymorphicFunctionTypeOverAliases(): Unit = checkTextHasNoErrors(
    """trait RIO[-R, +A]
      |trait LogEnv
      |trait SpaceId
      |object InfraEnvironment:
      |  type CTask[A] = RIO[LogEnv & SpaceId, A]
      |  type LTask[A] = RIO[LogEnv, A]
      |  type RunCTask = [T] => SpaceId => CTask[T] => LTask[T]
      |object business:
      |  import InfraEnvironment.{LTask, RunCTask}
      |  trait Control:
      |    def getCompanies(runCTask: RunCTask): LTask[List[String]]
      |  def mk: RunCTask = ???
      |""".stripMargin
  )

  def testAliasOfPolymorphicFunctionTypeFromAnotherFile(): Unit = {
    myFixture.addFileToProject("infra/InfraEnvironment.scala",
      """package infra
        |trait RIO[-R, +A]
        |trait LogEnv
        |trait SpaceId
        |object InfraEnvironment:
        |  type CTask[A] = RIO[LogEnv & SpaceId, A]
        |  type LTask[A] = RIO[LogEnv, A]
        |  type RunCTask = [T] => SpaceId => CTask[T] => LTask[T]
        |""".stripMargin)
    checkTextHasNoErrors(
      """package business
        |import infra.InfraEnvironment.{LTask, RunCTask}
        |trait Control:
        |  def getCompanies(runCTask: RunCTask): LTask[List[String]]
        |""".stripMargin
    )
  }
}
