package org.jetbrains.plugins.scala.lang.psi.implicits

import com.intellij.psi.PsiDocumentManager
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.{assertEquals, assertTrue}

import scala.jdk.CollectionConverters._

class ImplicitCollectorCacheRetentionTest extends ScalaLightCodeInsightFixtureTestCase {
  override def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3_0

  private def cache = ScalaPsiManager.instance(getProject).implicitCollectorCache

  private def errors: Seq[String] =
    myFixture.doHighlighting().asScala.filter(_.getSeverity.myVal >= 300).map(_.getDescription).toSeq

  def testEntriesSurviveLocalEdit(): Unit = {
    configureFromFileText(
      s"""object Ext { extension (i: Int) def twice: Int = i * 2 }
         |import Ext.*
         |object O {
         |  def a(): Int = { val r = 1.twice; $CARET r }
         |  def b(): Int = { val r = 2.twice; r }
         |  def c(): String = { val r = 3.twice; r.toString }
         |}
         |""".stripMargin)
    assertEquals(Seq.empty, errors)
    val populated = cache.size()
    assertTrue(s"cache should be populated, size = $populated", populated > 0)

    myFixture.`type`("val unrelated = 1; ")
    PsiDocumentManager.getInstance(getProject).commitAllDocuments()
    assertTrue(s"entries should survive an edit inside a typed def body, size = ${cache.size()}", cache.size() > 0)
    assertEquals(Seq.empty, errors)
  }

  def testLocalGivenInvalidatesEntry(): Unit = {
    configureFromFileText(
      s"""trait Render[A] { def render(a: A): String }
         |object Ext { extension [A](a: A)(using r: Render[A]) def rendered: String = r.render(a) }
         |import Ext.*
         |object O {
         |  def f(): String = {
         |    $CARET
         |    val s = 1.rendered
         |    s
         |  }
         |}
         |""".stripMargin)
    assertTrue("expected an error before the given exists", errors.nonEmpty)

    myFixture.`type`("given Render[Int] = (i: Int) => i.toString")
    PsiDocumentManager.getInstance(getProject).commitAllDocuments()
    assertEquals(Seq.empty, errors)
  }
}
