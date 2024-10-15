package steps.annotation

import scala.annotation.experimental
import scala.language.experimental.clauseInterleaving

@toString
@experimental
class Foo1(val a: Int, val b: String)

@toString
@experimental
class Foo2(a: Int, b: String)

@toString
@experimental
class Foo3(var a: Int, var b: String)

@toString
@experimental
class Foo4(a: Int, b: String)(c: Int)

@experimental
class AssertToStringBehaviour extends munit.FunSuite:

  test("@toString works with all kinds of classes"):
    assertEquals(Foo1(1, "hello").toString(), "Foo1(1, hello)")
    assertEquals(Foo2(1, "hello").toString(), "Foo2(1, hello)")
    assertEquals(Foo3(1, "hello").toString(), "Foo3(1, hello)")
    assertEquals(Foo4(1, "hello")(2).toString(), "Foo4(1, hello, 2)")

end AssertToStringBehaviour