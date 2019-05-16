package org.jetbrains.plugins.scala.lang.psi
package types
package api

import com.intellij.openapi.util.Computable
import com.intellij.util.containers.ContainerUtil
import org.jetbrains.plugins.scala.caches.RecursionManager

/**
  * @author adkozlov
  */
trait Equivalence[Tpe <: ScalaType] {
  typeSystem: TypeSystem[Tpe] =>

  private val guard = RecursionManager.RecursionGuard[CacheKey, ConstraintsResult[Tpe]](s"${typeSystem.name}.equivalence.guard")
  private val cache = ContainerUtil.newConcurrentMap[CacheKey, ConstraintsResult[Tpe]]()

  private val eval = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = false
  }

  final def equiv(lhs: Tpe, rhs: Tpe): Boolean = equiv(lhs, rhs, emptyConstraints).isRight

  /** Checks if two types are equivalent.  */
  def equiv(lhs: Tpe, rhs: Tpe, constraints: ConstraintSystem[Tpe] = emptyConstraints): ConstraintsResult[Tpe]

  def clearCache(): Unit = cache.clear()

  protected def equivPreventingRecursion(
    key:         CacheKey,
    equivalence: Computable[ConstraintsResult[Tpe]]
  ): ConstraintsResult[Tpe] = {
    val nowEval = eval.get()
    val fromCache =
      if (nowEval) null
      else {
        try {
          eval.set(true)
          cache.get(key)
        } finally {
          eval.set(false)
        }
      }

    fromCache match {
      case null if guard.checkReentrancy(key) => ConstraintsResult.Left
      case null =>
        val stackStamp = RecursionManager.markStack()

        guard.doPreventingRecursion(key, equivalence) match {
          case null => ConstraintsResult.Left
          case result =>
            if (!nowEval && stackStamp.mayCacheNow()) {
              try {
                eval.set(true)
                cache.put(key, result)
              } finally {
                eval.set(false)
              }
            }
            result
        }
      case cached => cached
    }
  }
}
