package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
final class toString extends MacroAnnotation:
  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = 
    import quotes.reflect.*
    val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")
    tree match
      case _: ClassDef if toStringSym.overridingSymbol(tree.symbol).exists =>
        report.warning(s"@toString is not necessary since hashcode is defined in ${tree.symbol}")
        List(tree)
      case ClassDef(className, ctr, parents, self, body) =>
        val cls = tree.symbol

        val fields = body.collect {
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            Select(This(cls), vdef.symbol).asExpr
        }

        val toStringOverrideSym = Symbol.newMethod(cls, "toString", toStringSym.info, Flags.Override, Symbol.noSymbol)

        def toStringOverrideDefBody(argss: List[List[Tree]]): Option[Term] =
          given Quotes = toStringOverrideSym.asQuotes
          Some(toStringExpr(className, fields).asTerm)

        val toStringDef = DefDef(toStringOverrideSym, toStringOverrideDefBody)
        List(ClassDef.copy(tree)(className, ctr, parents, self, toStringDef :: body))
      case _ =>
        report.errorAndAbort("@toString is only supported in class/object/trait")
  end transform

  private def toStringExpr(className: String, thisFields: List[Expr[Any]])(using Quotes): Expr[String] =
    val fieldsSeq = Expr.ofSeq(thisFields)
    val prefix = Expr(className + "(")
    '{ $fieldsSeq.mkString($prefix, ", ", ")") }
  end toStringExpr

end toString