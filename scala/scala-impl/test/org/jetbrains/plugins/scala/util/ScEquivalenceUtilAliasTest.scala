package org.jetbrains.plugins.scala.util

import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAlias
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScTypeExt}
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.{assertFalse, assertTrue}

/** Two copies of one alias (e.g. the JVM and Scala.js jars of a cross-built library) must be one type. */
class ScEquivalenceUtilAliasTest extends ScalaLightCodeInsightFixtureTestCase {

  override protected def defaultVersionOverride: Option[ScalaVersion] = Some(LatestScalaVersions.Scala_3_8)

  private def alias(path: String, text: String, name: String): ScTypeAlias = {
    val file = myFixture.addFileToProject(path, text).asInstanceOf[ScalaFile]
    PsiTreeUtil.findChildrenOfType(file, classOf[ScTypeAlias]).toArray(Array.empty[ScTypeAlias]).find(_.name == name).get
  }

  private def equivalent(a: PsiElement, b: PsiElement): Boolean = {
    implicit val ctx: Context = Context(a)
    ScEquivalenceUtil.smartEquivalence(a, b)
  }

  def testSameTopLevelAliasFromTwoRoots(): Unit = {
    val a = alias("a/Refined.scala", "package lib\nopaque type Refined[T, P] = T", "Refined")
    val b = alias("b/Refined.scala", "package lib\nopaque type Refined[T, P] = T", "Refined")
    assertTrue(equivalent(a, b))
    val useSite = myFixture.addFileToProject("app/Use.scala", "package app\nobject Use")
    implicit val ctx: Context = Context(useSite)
    assertTrue(ScDesignatorType(a).equiv(ScDesignatorType(b)))
  }

  def testSameMemberAliasFromTwoRoots(): Unit = {
    val a = alias("a/Types.scala", "package lib\nobject types:\n  type NES = String", "NES")
    val b = alias("b/Types.scala", "package lib\nobject types:\n  type NES = String", "NES")
    assertTrue(equivalent(a, b))
  }

  def testDifferentPackagesAreNotEquivalent(): Unit = {
    val a = alias("a/Refined.scala", "package lib1\nopaque type Refined[T, P] = T", "Refined")
    val b = alias("b/Refined.scala", "package lib2\nopaque type Refined[T, P] = T", "Refined")
    assertFalse(equivalent(a, b))
  }

  def testLocalAliasesAreNotEquivalent(): Unit = {
    val a = alias("a/Local.scala", "package lib\ndef f: Int =\n  type Local = Int\n  1", "Local")
    val b = alias("b/Local.scala", "package lib\ndef g: Int =\n  type Local = Int\n  1", "Local")
    assertFalse(equivalent(a, b))
  }

  def testDefinitionAndDeclarationAreNotEquivalent(): Unit = {
    val a = alias("a/T.scala", "package lib\nobject holder:\n  type T = Int", "T")
    val b = alias("b/T.scala", "package lib\ntrait holder:\n  type T", "T")
    assertFalse(equivalent(a, b))
  }
}
