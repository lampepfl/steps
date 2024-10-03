package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*
import scala.collection.mutable.Map
import scala.collection.concurrent.TrieMap

@experimental
class memoize extends MacroAnnotation:
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

  private def buildCache[K: Type, V: Type](name: String)(using Quotes): quotes.reflect.ValDef =
    import quotes.reflect.*
    val symbol = Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[Map[K, V]], Flags.Private, Symbol.spliceOwner)
    val rhs =
      given Quotes = symbol.asQuotes
      '{ TrieMap.empty[K, V] }.asTerm
    end rhs
    ValDef(symbol, Some(rhs))
  end buildCache

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