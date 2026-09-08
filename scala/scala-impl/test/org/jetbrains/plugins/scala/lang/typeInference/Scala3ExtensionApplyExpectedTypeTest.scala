package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

/** A value applied through an extension `apply` (scalajs-react 4 tags): branch arguments must expect the
  * extension's argument clause, not its receiver clause. */
class Scala3ExtensionApplyExpectedTypeTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  def testBranchingArgumentOfExtensionApply(): Unit = checkTextHasNoErrors(
    """trait TagMod
      |trait TopNode
      |trait HtmlTopNode extends TopNode
      |trait Div extends HtmlTopNode
      |class TagOf[+N <: TopNode] extends TagMod:
      |  def apply(xs: TagMod*): TagOf[N] = ???
      |trait TagLite[Top <: TopNode]:
      |  final opaque type Tag[+N <: Top] = String
      |  def apply[N <: Top](name: String): Tag[N] = ???
      |  extension [N <: Top](self: Tag[N])
      |    def apply(xs: TagMod*): TagOf[N] = ???
      |  implicit def toTagOf[N <: Top](t: Tag[N]): TagOf[N] = ???
      |object HtmlTagOf extends TagLite[HtmlTopNode]
      |trait VdomElement extends TagMod
      |trait Unmounted[P] extends VdomElement
      |object all:
      |  val div: HtmlTagOf.Tag[Div] = HtmlTagOf("div")
      |object use:
      |  import all.div
      |  def editor: Unmounted[String] = ???
      |  def f(b: Boolean): TagMod = div(if b then div() else editor)
      |  def g(b: Boolean): TagMod = div(b match
      |    case true  => div()
      |    case false => editor)
      |  def h: TagMod = div(editor)
      |  def k: TagMod = div(div(), editor)
      |""".stripMargin
  )
}
