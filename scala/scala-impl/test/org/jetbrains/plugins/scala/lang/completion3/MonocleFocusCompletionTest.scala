package org.jetbrains.plugins.scala.lang.completion3

import org.jetbrains.plugins.scala.lang.completion3.base.ScalaCompletionTestBase
import org.jetbrains.plugins.scala.util.runners.{RunWithScalaVersions, TestScalaVersion}
import org.junit.Test

@RunWithScalaVersions(Array(TestScalaVersion.Scala_3_Latest))
class MonocleFocusCompletionTest extends ScalaCompletionTestBase {

  private val monocleStub =
    """package monocle {
      |  trait Iso[S, A]
      |  trait Lens[S, A]
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

  @Test
  def testPlaceholderFieldCompletion(): Unit = doCompletionTest(
    fileText =
      s"""$monocleStub
         |case class State(activeLocation: Int, other: String)
         |object T { val lens = monocle.macros.GenLens[State](_.act$CARET) }
         |""".stripMargin,
    resultText =
      s"""$monocleStub
         |case class State(activeLocation: Int, other: String)
         |object T { val lens = monocle.macros.GenLens[State](_.activeLocation$CARET) }
         |""".stripMargin,
    item = "activeLocation"
  )

  @Test
  def testEmptyPlaceholderCompletion(): Unit = doCompletionTest(
    fileText =
      s"""$monocleStub
         |case class State(activeLocation: Int, other: String)
         |object T { val lens = monocle.macros.GenLens[State](_.$CARET) }
         |""".stripMargin,
    resultText =
      s"""$monocleStub
         |case class State(activeLocation: Int, other: String)
         |object T { val lens = monocle.macros.GenLens[State](_.other$CARET) }
         |""".stripMargin,
    item = "other"
  )
}
