package steps.quoted

import scala.quoted.*

extension(using Quotes)(inline cls: quotes.reflect.Symbol)

    /** Check if a given class, trait or object overrides a given definition
      *
      * @param cls The class, trait or object to inspect
      * @param sym The symbol that should be overriden
      * @return true if the definition is overriden, false otherwise
      */
    inline def overrides(inline sym: quotes.reflect.Symbol): Boolean =
        sym.overridingSymbol(cls).exists

end extension 
