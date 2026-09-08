package org.jetbrains.plugins.scala.lang.imports.unused

import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.jetbrains.plugins.scala.util.assertions.MatcherAssertionsExt

/** A value applied through an implicit conversion of its type (scalajs-react component constructors). */
class UnusedImportComponentCtorTest extends UnusedImportTestBase with MatcherAssertionsExt {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  private val library =
    """package react:
      |  sealed abstract class CtorType[-P, +U]
      |  object CtorType:
      |    final class PropsAndChildren[-P, +U] extends CtorType[P, U]:
      |      def apply(props: P)(children: Any*): U = ???
      |  object Generic:
      |    trait ComponentSimple[P, CT[-p, +u] <: CtorType[p, u], U]:
      |      def ctor: CT[P, U]
      |    implicit def toComponentCtor[P, CT[-p, +u] <: CtorType[p, u], U](c: ComponentSimple[P, CT, U]): CT[P, U] = c.ctor
      |  object ScalaFn:
      |    type Component[P, CT[-p, +u] <: CtorType[p, u]] = Generic.ComponentSimple[P, CT, Unit]
      |package component:
      |  import react.CtorType
      |  import react.ScalaFn.Component
      |  object FeatureFlagHider:
      |    val spaceFeatureFlagHider: Component[String, CtorType.PropsAndChildren] = ???
      |""".stripMargin

  def testImportUsedByValueAppliedThroughImplicitCtorConversion(): Unit = {
    val text = library +
      """package use:
        |  import component.FeatureFlagHider.spaceFeatureFlagHider
        |  def render: Unit = spaceFeatureFlagHider("key")("child")
        |""".stripMargin
    assertMatches(messages(text)) {
      case Nil =>
    }
  }

  def testImportUsedByValueReferencedWithoutApplication(): Unit = {
    val text = library +
      """package use:
        |  import component.FeatureFlagHider.spaceFeatureFlagHider
        |  def render: Any = spaceFeatureFlagHider
        |""".stripMargin
    assertMatches(messages(text)) {
      case Nil =>
    }
  }
}
