package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*
import scala.collection.mutable.Map
import scala.collection.concurrent.TrieMap

/**
 * A macro annotation that automatically memoizes the result of a function based on its single input parameter.
 * 
 * The `@memoize` annotation can be applied to a method (def) that accepts exactly one parameter (no type parameters)
 * and returns a value. This annotation optimizes repeated calls to the function by storing previously computed results
 * in a cache. On subsequent calls with the same argument, the cached result is returned instead of recomputing the value.
 * 
 * The generated cache is a `scala.collection.concurrent.TrieMap`, and the key is the method parameter while the value
 * is the method return value. If the function has already been called with a particular argument, the cached result is 
 * returned; otherwise, the function is evaluated and its result is stored in the cache.
 *
 * Example usage:
 * {{{
 * @memoize
 * def fibonacci(n: Int): Int = 
 *   if (n <= 1) n
 *   else fibonacci(n - 1) + fibonacci(n - 2)
 *
 * fibonacci(10) // The result will be cached for future calls with the same argument.
 * }}}
 * 
 * The above example will cache results for each input `n`, reducing the number of recursive calls and improving performance.
 *
 * @note This annotation requires Scala's `experimental` flag to be enabled, or it can be used within an experimental scope 
 *       (i.e., `import scala.annotation.experimental`), as it uses experimental macro annotation features.
 */
@experimental
class memoize extends MacroAnnotation:

  /**
   * Transforms the annotated method into one that utilizes memoization.
   * 
   * The macro generates a cache and wraps the original method body in logic that checks the cache before evaluating 
   * the method. If the result for the given argument exists in the cache, it is returned directly; otherwise, the method 
   * body is evaluated and the result is stored in the cache.
   * 
   * @param definition The abstract syntax tree (AST) of the annotated method.
   * @return The transformed method with memoization logic.
   */
  def transform(using Quotes)(definition: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    definition match
      case DefDef(name, params@List(TermParamClause(List(param))), tpt, Some(rhs)) =>
        (param.tpt.tpe.asType, tpt.tpe.asType) match
          case ('[k], '[v]) =>
            val cache = buildCache[k,v](Symbol.freshName(s"${name}Cache"))
            val newRHS = 
              adaptDefToCache(using definition.symbol.asQuotes)
                             (Ref(cache.symbol).asExprOf[Map[k, v]], 
                              Ref(param.symbol).asExprOf[k], 
                              rhs.asExprOf[v]
                              )
            List(cache, DefDef.copy(definition)(name, params, tpt, Some(newRHS.asTerm)))
      case _ =>
        report.error("@memoize is only allowed on def with a single parameter and no type parameters")
        List(definition)
  end transform

  /**
   * Creates a cache (TrieMap) to store the memoized results.
   *
   * The cache is defined as a private `TrieMap[K, V]` inside the class where the memoized method is defined.
   * 
   * @param name The name of the cache variable.
   * @tparam K The type of the method's input parameter (key in the cache).
   * @tparam V The return type of the method (value in the cache).
   * @return A `ValDef` that defines the cache variable.
   */
  private def buildCache[K: Type, V: Type](name: String)(using Quotes): quotes.reflect.ValDef =
    import quotes.reflect.*
    val symbol = Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[Map[K, V]], Flags.Private, Symbol.spliceOwner)
    val rhs =
      given Quotes = symbol.asQuotes
      '{ TrieMap.empty[K, V] }.asTerm
    end rhs
    ValDef(symbol, Some(rhs))
  end buildCache

  /**
   * Adapts the original method to utilize the cache.
   *
   * This function wraps the original method body with logic that first checks if the result for a given input 
   * is already in the cache. If so, the cached result is returned; if not, the method body is evaluated and 
   * the result is stored in the cache for future use.
   * 
   * @param cache A reference to the cache storing memoized results.
   * @param key The input parameter of the method (the key in the cache).
   * @param rhs The original method body (the computation to memoize).
   * @tparam K The type of the method's input parameter.
   * @tparam V The return type of the method.
   * @return An expression that checks the cache before evaluating the method body.
   */
  private def adaptDefToCache[K: Type, V: Type](using Quotes)(cache: Expr[Map[K, V]], key: Expr[K], rhs: Expr[V]): Expr[V] =
    import quotes.reflect.*
    '{ 
        if $cache.contains($key) then 
          $cache($key)
        else
          val res = $rhs
          $cache($key) = res
          res
      }
  end adaptDefToCache

end memoize