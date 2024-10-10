package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
final class equals extends MacroAnnotation:

  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val equalsSym = Symbol.requiredMethod("java.lang.Object.equals")
    tree match
      case _: ClassDef if equalsSym.overridingSymbol(tree.symbol).exists =>
        report.warning(s"@equals is not necessary since hashcode is defined in ${tree.symbol}")
        List(tree)
      case ClassDef(className, ctr, parents, self, body) =>
        val cls = tree.symbol
        val equalsOverrideSym = Symbol.newMethod(cls, "equals", equalsSym.info, Flags.Override | Flags.Final, Symbol.noSymbol)

        val fields = body.collect {
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            Select(This(cls), vdef.symbol).asExpr
        }

        def equalsOverrideDefBody(argss: List[List[Tree]]): Option[Term] =
          import quotes.reflect.*
          given Quotes = equalsOverrideSym.asQuotes
          cls.typeRef.asType match
            case '[c] =>
              Some(equalsExpr[c](argss.head.head.asExpr, fields).asTerm)
        end equalsOverrideDefBody

        val equalsDefDef = DefDef(equalsOverrideSym, equalsOverrideDefBody)
        List(ClassDef.copy(tree)(className, ctr, parents, self, equalsDefDef :: body))
      case _ =>
        report.errorAndAbort("@equals is only supported in class/object/trait")
  end transform




  private def equalsExpr[T: Type](that: Expr[Any], thisFields: List[Expr[Any]])(using Quotes): Expr[Boolean] =
    '{
      $that match
        case that: T @unchecked =>
          ${
            val thatFields: List[Expr[Any]] =
              import quotes.reflect.*
              thisFields.map(field => Select('{that}.asTerm, field.asTerm.symbol).asExpr)
            thisFields.zip(thatFields)
              .map { case (thisField, thatField) => '{ $thisField == $thatField } }
              .reduce { case (pred1, pred2) => '{ $pred1 && $pred2 } }
          }
        case _ => false
    }
  end equalsExpr

end equals