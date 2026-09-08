package org.jetbrains.plugins.scala.lang.psi.implicits

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.caches.{BlockModificationTracker, RecursionManager}
import org.jetbrains.plugins.scala.caches.stats.Tracer
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScExtension, ScFunction}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.resolve.ScalaResolveResult
import org.jetbrains.plugins.scala.util.HashBuilder.toHashBuilder

import java.util.concurrent.ConcurrentHashMap

/**
 * Search results are kept across edits that cannot change them: every entry is stamped with the block modification
 * count of its search scope, which changes when anything inside that block or an enclosing block is edited and
 * on every top-level change (signatures, imports, members). Candidate definitions can only change through top-level
 * edits, so an entry with a matching stamp is still valid. The cache is cleared as a whole on top-level changes.
 */
class ImplicitCollectorCache(project: Project) {
  private final case class Cached(stamp: Long, results: Seq[ScalaResolveResult])

  private val map =
    new ConcurrentHashMap[(ImplicitSearchScope, ScType), Cached]()

  private val nonValueTypesMap =
    new ConcurrentHashMap[NonValueTypesKey, NonValueFunctionTypes]()

  private def stamp(scope: ImplicitSearchScope): Long =
    BlockModificationTracker(scope.representative).getModificationCount

  def get(place: PsiElement, tp: ScType): Option[Seq[ScalaResolveResult]] = {
    val scope = ImplicitSearchScope.forElement(place)
    Option(map.get((scope, tp))).filter(_.stamp == stamp(scope)).map(_.results)
  }

  def getOrCompute(place: PsiElement, tp: ScType, mayCacheResult: Boolean)
                  (computation: => Seq[ScalaResolveResult]): Seq[ScalaResolveResult] = {

    val tracer = Tracer("ImplicitCollectorCache.getOrCompute", "ImplicitCollectorCache.getOrCompute")
    tracer.invocation()

    get(place, tp) match {
      case Some(cached) => return cached
      case _ =>
    }

    tracer.calculationStart()
    try {

      val stackStamp = RecursionManager.markStack()
      val result = computation

      if (mayCacheResult && stackStamp.mayCacheNow())
        put(place, tp, result)

      result

    } finally {
      tracer.calculationEnd()
    }
  }

  private[implicits] def getNonValueTypes(
    fun:                 ScFunction,
    substitutor:         ScSubstitutor,
    exportedInExtension: Option[ScExtension],
    typeFromMacro:       Option[ScType]
  ): NonValueFunctionTypes = {

    val key = NonValueTypesKey(fun, substitutor, exportedInExtension, typeFromMacro)

    nonValueTypesMap.computeIfAbsent(
      key,
      key =>
        NonValueFunctionTypes(
          key.fun,
          key.substitutor,
          key.exportedInExtension,
          key.typeFromMacro
        )
    )
  }

  def put(place: PsiElement, tp: ScType, value: Seq[ScalaResolveResult]): Unit = {
    val scope = ImplicitSearchScope.forElement(place)
    map.put((scope, tp), Cached(stamp(scope), value))
  }

  def size(): Int = map.size() + nonValueTypesMap.size()

  def clear(): Unit = {
    map.clear()
    nonValueTypesMap.clear()
  }

  private case class NonValueTypesKey(
    fun:                 ScFunction,
    substitutor:         ScSubstitutor,
    exportedInExtension: Option[ScExtension],
    typeFromMacro:       Option[ScType]
  ) {
    override def hashCode(): Int =
      fun.hashCode() #+
        substitutor.hashCode() #+
        exportedInExtension #+
        typeFromMacro

    override def equals(obj: Any): Boolean = obj match {
      case NonValueTypesKey(otherFun, otherSubst, otherExportedInExtension, otherType) =>
        otherFun == fun && substitutor == otherSubst &&
          exportedInExtension == otherExportedInExtension &&
          typeFromMacro == otherType
      case _ =>
        false
    }
  }
}
