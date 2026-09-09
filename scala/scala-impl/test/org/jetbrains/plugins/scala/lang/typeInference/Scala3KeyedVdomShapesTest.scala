package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

class Scala3KeyedVdomShapesTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  private val keyed =
    """trait Show[A]
      |object Show:
      |  given Show[Int] = ???
      |  given Show[String] = ???
      |trait Node
      |object vdom:
      |  extension [A](as: IterableOnce[A])
      |    def toKeyedVdom[K: Show](extractKey: A => K)(f: A => Node): Node = ???
      |import vdom.*
      |enum Color:
      |  case Red, Green
      |def node(s: String): Node = ???
      |""".stripMargin

  def testExtensionOnEnumValuesArray(): Unit = checkTextHasNoErrors(
    keyed + "val a: Node = Color.values.toKeyedVdom(_.ordinal) { c => node(c.toString) }\n"
  )

  def testExtensionOnArrayLiteral(): Unit = checkTextHasNoErrors(
    keyed + "val b: Node = Array(\"x\", \"y\").toKeyedVdom(s => s) { s => node(s) }\n"
  )

  def testExtensionOnListControl(): Unit = checkTextHasNoErrors(
    keyed + "val c: Node = List(\"x\", \"y\").toKeyedVdom(s => s) { s => node(s) }\n"
  )

  def testOpsConversionInNewtypePrefixObject(): Unit = checkTextHasNoErrors(
    """object Impl:
      |  type Base
      |  trait Tag[+A]
      |  type Type[+A] <: Base with Tag[A]
      |  final class Ops[A](val value: Type[A]):
      |    def toList: List[A] = ???
      |  implicit def ops[A](value: Type[A]): Ops[A] = new Ops(value)
      |type NEC[+A] = Impl.Type[A]
      |def f(e: NEC[Int]): List[Int] = e.toList
      |def g(e: Impl.Type[Int]): List[Int] = e.toList
      |""".stripMargin
  )

  def testMemberInheritedFromHigherKindedParentOfNewtypeOps(): Unit = checkTextHasNoErrors(
    """trait Chain[+A]
      |trait NonEmptyCollection[+A, U[+_], NE[+_]]:
      |  def toList: List[A]
      |  def length: Long
      |object Impl:
      |  type Base
      |  trait Tag[+A]
      |  type Type[+A] <: Base with Tag[A]
      |  final class Ops[A](private val value: Type[A]) extends AnyVal with NonEmptyCollection[A, Chain, NEC]:
      |    def toList: List[A] = ???
      |    def length: Long = ???
      |  implicit def ops[A](value: NEC[A]): Ops[A] = new Ops(value)
      |type NEC[+A] = Impl.Type[A]
      |def f(e: NEC[Int]): List[Int] = e.toList
      |def g(e: NEC[Int]): Long = e.length
      |""".stripMargin
  )

  private val foldableOverNewtype =
    """trait Foldable[F[_]]:
      |  def toList[A](fa: F[A]): List[A]
      |object Impl:
      |  type Base
      |  trait Tag[+A]
      |  type Type[+A] <: Base with Tag[A]
      |  given Foldable[NEC] = ???
      |type NEC[+A] = Impl.Type[A]
      |""".stripMargin

  def testFoldableExtensionSyntaxOverAliasedNewtype(): Unit = checkTextHasNoErrors(
    foldableOverNewtype +
      """object syntax:
        |  extension [F[_], A](fa: F[A])(using F: Foldable[F]) def toList: List[A] = F.toList(fa)
        |import syntax.*
        |def f(e: NEC[Int]): List[Int] = e.toList
        |""".stripMargin
  )

  def testFoldableImplicitClassSyntaxOverAliasedNewtype(): Unit = checkTextHasNoErrors(
    foldableOverNewtype +
      """object syntax:
        |  implicit final class FoldableOps[F[_], A](private val fa: F[A]) extends AnyVal:
        |    def toList(implicit F: Foldable[F]): List[A] = F.toList(fa)
        |import syntax.*
        |def f(e: NEC[Int]): List[Int] = e.toList
        |""".stripMargin
  )

  def testFoldableSummonOverAliasedNewtype(): Unit = checkTextHasNoErrors(
    foldableOverNewtype + "def f(e: NEC[Int]): List[Int] = summon[Foldable[NEC]].toList(e)\nval fi: Foldable[Impl.Type] = summon[Foldable[NEC]]\n"
  )
}
