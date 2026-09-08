package org.jetbrains.plugins.scala.lang.macros

import org.jetbrains.plugins.scala.lang.typeInference.TypeInferenceTestBase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

class MonocleFocusApplyTest extends TypeInferenceTestBase {
  override def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3_0

  private val monocleStub =
    """package monocle {
      |  trait Iso[S, A]
      |  trait Lens[S, A] { def get(s: S): A; def replace(a: A): S => S }
      |  object Focus {
      |    sealed trait KeywordContext
      |    def apply[S]: MkFocus[S] = ???
      |    class MkFocus[From] {
      |      def apply(): Iso[From, From] = ???
      |      transparent inline def apply[To](inline lambda: KeywordContext ?=> From => To): Any = ???
      |    }
      |  }
      |}
      |package monocle.macros {
      |  object GenLens { def apply[A]: monocle.Focus.MkFocus[A] = ??? }
      |}
      |""".stripMargin

  def testGenLensNestedPath(): Unit = doTest(
    s"""$monocleStub
       |case class Inner(count: Int)
       |case class State(inner: Inner, name: String)
       |object T {
       |  val lens = ${START}monocle.macros.GenLens[State](_.inner.count)$END
       |}
       |//Lens[State, Int]
       |""".stripMargin
  )

  def testFocusLambda(): Unit = doTest(
    s"""$monocleStub
       |case class State(name: String)
       |object T {
       |  val lens = ${START}monocle.Focus[State](s => s.name)$END
       |  val v: String = lens.get(State("x"))
       |}
       |//Lens[State, String]
       |""".stripMargin
  )
}
