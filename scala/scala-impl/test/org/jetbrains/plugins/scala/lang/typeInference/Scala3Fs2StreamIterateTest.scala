package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** `Stream.iterate[F[x] >: Pure[x], A](start)(f)` leaves `F` to the lower bound and `A` to the second clause;
 * the paginating streams in the POS adaptors are built on it.
 */
abstract class Scala3Fs2StreamIterateTestBase extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  protected def fs2Definitions: String

  def testIterateThenEvalMap(): Unit = checkTextHasNoErrors(
    s"""$fs2Definitions
       |import fs2.Stream
       |trait Z[-R, +E, +A]
       |type LTask[A] = Z[Any, Throwable, A]
       |def fetch(page: Int): LTask[List[Int]] = ???
       |val s: Stream[LTask, List[Int]] = Stream.iterate(1)(_ + 1).evalMap(page => fetch(page)).takeWhile(_.nonEmpty)
       |val t: Stream[LTask, Int] = Stream.iterate(1)(_ + 1).evalMap(fetch).map(_.size)
       |""".stripMargin
  )
}

class Scala3Fs2StreamIterateTest extends Scala3Fs2StreamIterateTestBase {
  override protected def fs2Definitions: String =
    """package fs2:
      |  type Pure[A] = Nothing
      |  trait Stream[+F[_], +O]:
      |    def evalMap[F2[x] >: F[x], O2](f: O => F2[O2]): Stream[F2, O2] = ???
      |    def takeWhile(p: O => Boolean): Stream[F, O] = ???
      |    def map[O2](f: O => O2): Stream[F, O2] = ???
      |  object Stream:
      |    def iterate[F[x] >: Pure[x], A](start: A)(f: A => A): Stream[F, A] = ???
      |""".stripMargin
}

class Scala3Fs2StreamIterateRealLibraryTest extends Scala3Fs2StreamIterateTestBase {
  override protected def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader("co.fs2" %% "fs2-core" % "3.13.0")

  override protected def fs2Definitions: String = ""
}
