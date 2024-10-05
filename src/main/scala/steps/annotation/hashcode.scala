package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
final class hashcode extends MacroAnnotation:

  override def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = 
    import quotes.reflect.*
    val hashCodeSym = Symbol.requiredMethod("java.lang.Object.hashCode")
    tree match
      case _: ClassDef if hashCodeSym.overridingSymbol(tree.symbol).exists => 
        report.warning(s"@hashcode is not necessary since hashcode is defined in ${tree.symbol}")
        List(tree)
      case ClassDef(className, ctr, parents, self, body) =>
        val cls = tree.symbol

        val fields = body.collect:
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            Select(This(cls), vdef.symbol).asExpr
        end fields

        val hashCodeOverrideSym = Symbol.newMethod(cls, hashCodeSym.name, hashCodeSym.info, Flags.Override | Flags.Final, Symbol.noSymbol)
        val hashCodeOverrideDef = DefDef(hashCodeOverrideSym, _ =>
          given Quotes = hashCodeOverrideSym.asQuotes
          Some(hashCodeExpr(className, fields).asTerm)
        )

        List(ClassDef.copy(tree)(className, ctr, parents, self, hashCodeOverrideDef  :: body))
      case _ => 
        report.errorAndAbort("@hashcode is only supported in class/object/trait")
    
  end transform

  private def hashCodeExpr(className: String, thisFields: List[Expr[Any]])(using Quotes): Expr[Int] =
  '{
    var acc: Int = ${ Expr(scala.runtime.Statics.mix(-889275714, className.hashCode)) }
    ${
      Expr.block(
        thisFields.map {
          case '{ $field: Boolean } => '{ if $field then 1231 else 1237 }
          case '{ $field: Byte } => '{ $field.toInt }
          case '{ $field: Char } => '{ $field.toInt }
          case '{ $field: Short } => '{ $field.toInt }
          case '{ $field: Int } => field
          case '{ $field: Long } => '{ scala.runtime.Statics.longHash($field) }
          case '{ $field: Double } => '{ scala.runtime.Statics.doubleHash($field) }
          case '{ $field: Float } => '{ scala.runtime.Statics.floatHash($field) }
          case '{ $field: Null } => '{ 0 }
          case '{ $field: Unit } => '{ 0 }
          case field => '{ scala.runtime.Statics.anyHash($field) }
        }.map(hash => '{ acc = scala.runtime.Statics.mix(acc, $hash) }),
        '{ scala.runtime.Statics.finalizeHash(acc, ${Expr(thisFields.size)}) }
      )
    }
  }
  end hashCodeExpr


end hashcode