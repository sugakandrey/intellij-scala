package org.jetbrains.plugins.scala.lang.psi
package types
package api

import com.intellij.openapi.util.Computable
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.plugins.scala.caches.RecursionManager

/**
  * @author adkozlov
  */
trait Conformance[Tpe <: ScalaType] {
  typeSystem: TypeSystem[Tpe] =>

  import ConstraintsResult.Left

  private val guard = RecursionManager.RecursionGuard[CacheKey, ConstraintsResult[Tpe]](s"${typeSystem.name}.conformance.guard")
  private val cache = ContainerUtil.newConcurrentMap[CacheKey, ConstraintsResult[Tpe]]()

  /**
    * Checks if the conformance relation (<:, "is subtype of"), as defined by SLS 3.5.2, holds.
    */
  def conforms(lhs: Tpe, rhs: Tpe, constraints: ConstraintSystem[Tpe]): ConstraintsResult[Tpe]

  def clearCache(): Unit = cache.clear()

  protected def conformsPreventingRecursion[A](
    key:              CacheKey,
    conformanceCheck: Computable[ConstraintsResult[Tpe]]
  ): ConstraintsResult[Tpe] =
    cache.get(key) match {
      case null if guard.checkReentrancy(key) => Left
      case null =>
        val stackStamp = RecursionManager.markStack()

        guard.doPreventingRecursion(key, conformanceCheck) match {
          case null => Left
          case result =>
            if (stackStamp.mayCacheNow()) cache.put(key, result)
            result
        }
      case cached => cached
    }
}
