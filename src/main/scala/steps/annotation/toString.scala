package steps.annotation

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

/** A macro annotation that automatically generates a custom `toString` method
  * for a class.
  *
  * The `@toString` annotation can be applied to a class, object, or trait. When
  * applied, it overrides the `toString` method to include the class name and
  * the values of all fields marked as `ParamAccessor`.
  *
  * If the class already defines or overrides the `toString` method, the
  * annotation will emit a warning indicating that the annotation is not
  * necessary. The existing `toString` method will remain unchanged.
  *
  * Example usage:
  * {{{
  * @toString
  * class MyClass(val a: Int, val b: String)
  *
  * val instance = MyClass(1, "hello")
  * println(instance.toString) // Output: MyClass(1, hello)
  * }}}
  *
  * The generated `toString` method produces output in the format:
  * `ClassName(field1, field2, ...)`.
  *
  * @note
  *   This annotation requires Scala's `experimental` flag to be enabled, or it
  *   can be used within a scope marked as experimental (i.e., using `import
  *   scala.annotation.experimental`). This is necessary because it relies on
  *   experimental macro annotation features.
  */
@experimental
final class toString extends MacroAnnotation:

  /** Transforms the annotated class to add a custom `toString` method.
    *
    * If the class already overrides `toString`, a warning is emitted and no
    * changes are made. Otherwise, this annotation adds a new `toString` method
    * that returns a string representation of the class name and its fields.
    *
    * @param tree
    *   The abstract syntax tree (AST) of the annotated class, object, or trait.
    * @return
    *   The transformed class definition, with the generated `toString` method
    *   if applicable.
    */
  override def transform(using quotes: Quotes)(
      tree: quotes.reflect.Definition,
      _companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    val toStringSym = Symbol.requiredMethod("java.lang.Object.toString")
    tree match
      case _: ClassDef if toStringSym.overridingSymbol(tree.symbol).exists =>
        report.warning(
          s"@toString is not necessary since toString is defined in ${tree.symbol}"
        )
        List(tree)
      case cls: ClassDef if cls.symbol.flags.is(Flags.Trait) =>
        report.error(s"@toString is not supported on traits")
        List(tree)
      case cls: ClassDef if cls.symbol.flags.is(Flags.Module) =>
        report.error(s"@toString is not supported on objects")
        List(tree)
      case ClassDef(className, ctr, parents, self, body) =>
        val cls = tree.symbol

        val fields = body.collect {
          case vdef: ValDef if vdef.symbol.flags.is(Flags.ParamAccessor) =>
            Select(This(cls), vdef.symbol).asExpr
        }

        val toStringOverrideSym = Symbol.newMethod(
          cls,
          toStringSym.name,
          toStringSym.info,
          Flags.Override,
          Symbol.noSymbol
        )

        def toStringOverrideDefBody(argss: List[List[Tree]]): Option[Term] =
          given Quotes = toStringOverrideSym.asQuotes
          Some(toStringExpr(className, fields).asTerm)

        val toStringDef = DefDef(toStringOverrideSym, toStringOverrideDefBody)
        List(
          ClassDef
            .copy(tree)(className, ctr, parents, self, toStringDef :: body)
        )
      case _ =>
        report.errorAndAbort("@toString is only supported on class")
  end transform

  /** Helper method to create the string representation of the class.
    *
    * Constructs a string in the format: `ClassName(field1, field2, ...)`.
    *
    * @param className
    *   The name of the class.
    * @param thisFields
    *   The list of expressions representing the class fields.
    * @return
    *   A quoted expression representing the final string.
    */
  private def toStringExpr(className: String, thisFields: List[Expr[Any]])(using
      Quotes
  ): Expr[String] =
    val fieldsSeq = Expr.ofSeq(thisFields)
    val prefix = Expr(className + "(")
    '{ $fieldsSeq.mkString($prefix, ", ", ")") }
  end toStringExpr

end toString
