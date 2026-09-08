package org.jetbrains.plugins.scala.codeInspection.annotator

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.lang.annotation.{AnnotationSession, HighlightSeverity}
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.annotator.{DummyScalaAnnotationBuilder, ScalaAnnotationBuilder, ScalaAnnotationHolder, ScalaAnnotator}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import scala.annotation.nowarn

/**
 * Reports every error the Scala annotator produces as an inspection problem, so headless `inspect` runs see them
 * all: the platform's annotator-based inspection drops annotations that carry a problem group, which hides every
 * type mismatch. Meant for batch sweeps only, hence off by default.
 */
final class ScalaAnnotatorErrorsInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = new PsiElementVisitor {
    override def visitElement(element: PsiElement): Unit = element.getContainingFile match {
      case file: ScalaFile if element.getLanguage.isKindOf(ScalaLanguage.INSTANCE) =>
        val collector = new CollectingHolder(file)
        new ScalaAnnotator().annotate(element, typeAware = true)(collector)
        val elementRange = element.getTextRange
        collector.errors.foreach { case (range, message) =>
          val inElement = Option(range.intersection(elementRange)).filter(!_.isEmpty)
            .getOrElse(elementRange)
            .shiftLeft(elementRange.getStartOffset)
          holder.registerProblem(element, message, ProblemHighlightType.GENERIC_ERROR, inElement)
        }
      case _ =>
    }
  }

  private final class CollectingHolder(file: ScalaFile) extends ScalaAnnotationHolder {
    private val collected = List.newBuilder[(TextRange, String)]

    def errors: List[(TextRange, String)] = collected.result()

    //noinspection ApiStatus,UnstableApiUsage
    override def getCurrentAnnotationSession: AnnotationSession = new AnnotationSession(file): @nowarn("cat=deprecation")

    override def isBatchMode: Boolean = true

    override def newAnnotation(severity: HighlightSeverity, message: String): ScalaAnnotationBuilder =
      new Builder(severity, message)

    override def newSilentAnnotation(severity: HighlightSeverity): ScalaAnnotationBuilder =
      new Builder(severity, null)

    private final class Builder(severity: HighlightSeverity, @Nls message: String)
      extends DummyScalaAnnotationBuilder(severity, message) {
      override def onCreate(severity: HighlightSeverity, message: String, range: TextRange,
                            enforcedAttributes: TextAttributesKey, fixes: Seq[com.intellij.codeInsight.intention.CommonIntentionAction]): Unit =
        if (severity == HighlightSeverity.ERROR && message != null) collected += ((range, message))
    }
  }
}
