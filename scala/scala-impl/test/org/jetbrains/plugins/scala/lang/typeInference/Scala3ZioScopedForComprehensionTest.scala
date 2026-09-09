package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

abstract class Scala3ZioScopedForComprehensionTestBase extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  protected def zioDefinitions: String

  def testScopedAroundForComprehension(): Unit = checkTextHasNoErrors(
    s"""$zioDefinitions
       |import zio.*
       |trait LogEnv
       |type LTask[A] = RIO[LogEnv, A]
       |def use: ZIO[LogEnv, Throwable, Unit] = ???
       |def reader: Task[java.io.StringReader] = ???
       |def withReader: LTask[Unit] =
       |  ZIO.scoped:
       |    for
       |      channel <- ZIO.fromAutoCloseable(reader)
       |      result  <- use
       |    yield result
       |""".stripMargin
  )

  def testScopedAroundForComprehensionWithAbstractEnvironment(): Unit = checkTextHasNoErrors(
    s"""$zioDefinitions
       |import zio.*
       |def reader: Task[java.io.StringReader] = ???
       |def withReader[R, A](use: ZIO[R, Throwable, A]): ZIO[R, Throwable, A] =
       |  ZIO.scoped:
       |    for
       |      channel <- ZIO.fromAutoCloseable(reader)
       |      result  <- use
       |    yield result
       |""".stripMargin
  )

  def testScopedAroundPlainEffect(): Unit = checkTextHasNoErrors(
    s"""$zioDefinitions
       |import zio.*
       |trait LogEnv
       |type LTask[A] = RIO[LogEnv, A]
       |def use: ZIO[LogEnv & Scope, Throwable, Unit] = ???
       |def direct: LTask[Unit] = ZIO.scoped(use)
       |def colon: LTask[Unit] = ZIO.scoped:
       |  use
       |""".stripMargin
  )
}

class Scala3ZioScopedForComprehensionTest extends Scala3ZioScopedForComprehensionTestBase {
  override protected def zioDefinitions: String =
    """package zio:
      |  trait Trace
      |  object Trace:
      |    implicit def newTrace: Trace = ???
      |  trait Scope
      |  trait ZIO[-R, +E, +A]:
      |    def flatMap[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B])(implicit trace: Trace): ZIO[R1, E1, B] = ???
      |    def map[B](f: A => B)(implicit trace: Trace): ZIO[R, E, B] = ???
      |  object ZIO:
      |    def scoped[R]: ScopedPartiallyApplied[R] = new ScopedPartiallyApplied[R]
      |    def fromAutoCloseable[R, E, A <: AutoCloseable](fa: => ZIO[R, E, A])(implicit trace: Trace): ZIO[R & Scope, E, A] = ???
      |    final class ScopedPartiallyApplied[R](private val dummy: Boolean = true) extends AnyVal:
      |      def apply[E, A](zio: => ZIO[Scope & R, E, A])(implicit trace: Trace): ZIO[R, E, A] = ???
      |  type RIO[-R, +A] = ZIO[R, Throwable, A]
      |  type Task[+A] = ZIO[Any, Throwable, A]
      |""".stripMargin
}

class Scala3ZioScopedForComprehensionRealLibraryTest extends Scala3ZioScopedForComprehensionTestBase {
  override protected def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader("dev.zio" %% "zio" % "2.1.26")

  override protected def zioDefinitions: String = ""
}

class Scala3ZioVarianceStubTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  def testFlatMapWithBoundedContravariantTypeParameter(): Unit = checkTextHasNoErrors(
    """trait LogEnv
      |trait HasSpaceId
      |trait ZIO[-R, +E, +A]:
      |  def flatMap[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = ???
      |  def map[B](f: A => B): ZIO[R, E, B] = ???
      |def wide: ZIO[LogEnv & HasSpaceId, Throwable, Int] = ???
      |def narrow: ZIO[LogEnv, Throwable, Unit] = ???
      |def direct: ZIO[LogEnv & HasSpaceId, Throwable, Unit] = wide.flatMap(_ => narrow)
      |def comprehension: ZIO[LogEnv & HasSpaceId, Throwable, Unit] =
      |  for
      |    a <- wide
      |    _ <- narrow
      |  yield ()
      |""".stripMargin
  )

  def testTraverseUnderscoreInZioForComprehension(): Unit = checkTextHasNoErrors(
    """trait LogEnv
      |trait HasSpaceId
      |trait ZIO[-R, +E, +A]:
      |  def flatMap[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] = ???
      |  def map[B](f: A => B): ZIO[R, E, B] = ???
      |trait Applicative[G[_]]
      |object interop:
      |  given zioApplicative[R, E]: Applicative[[a] =>> ZIO[R, E, a]] = ???
      |object syntax:
      |  extension [A](xs: List[A])
      |    def traverse_[G[_]: Applicative, B](f: A => G[B]): G[Unit] = ???
      |import interop.given
      |import syntax.*
      |def upload(s: String): ZIO[LogEnv, Throwable, Unit] = ???
      |def wide: ZIO[LogEnv & HasSpaceId, Throwable, Int] = ???
      |def comprehension(images: List[String]): ZIO[LogEnv & HasSpaceId, Throwable, Unit] =
      |  for
      |    a <- wide
      |    _ <- images.traverse_(img => upload(img))
      |  yield ()
      |def direct(images: List[String]): ZIO[LogEnv, Throwable, Unit] = images.traverse_(img => upload(img))
      |""".stripMargin
  )
}
