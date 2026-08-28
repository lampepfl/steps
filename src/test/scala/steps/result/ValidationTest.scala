package steps.testing

import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import steps.result.Validation
import ValidationTest.ParseErr
import ValidationTest.abstracted

private val prelude = """
package steps.testing
import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import steps.result.Validation
import caps.any
import caps.fresh
import ValidationTest.ParseErr
import ValidationTest.Escaped
import ValidationTest.abstracted

"""

def expr(expr: String) = s"""
$prelude

def Test = {
  ${expr.linesWithSeparators.map(line => "  " + line).mkString}
}
"""

class ValidationTest extends munit.FunSuite {
  import opaques.*

  test("foo") {
    // FIXME: WTF!!!!
    val escape: Result[Validation[Any], List[Any]] = Validation.validate { v =>
      v
    }
    escape match {
      case Result.Ok(v: Validation[Any]^) =>
        v.test(false, "leaked!")
        assertEquals(v.snapshot, List("leaked!"))
      case Result.Err(errs) => assertEquals(errs, Nil)
    }
  }

  object opaques:
    opaque type PosInt = Int
    object PosInt:
      def test(i: Int): Result[PosInt, ParseErr] =
        if i > 0 then Result.Ok(i)
        else Result.Err(ParseErr("i must be positive"))

      inline def apply(i: Int): PosInt = inline i match
        case _: scala.compiletime.ops.int.S[?] => i

    opaque type NonEmptyStr = String
    object NonEmptyStr:
      def test(s: String): Result[NonEmptyStr, ParseErr] =
        if s.nonEmpty then Result.Ok(s)
        else Result.Err(ParseErr("s must be non-empty"))

      inline def apply(s: String): NonEmptyStr = inline compiletime.constValue[s.type] match
        case _: "" => compiletime.error("s must be non-empty")
        case _ => s

  test("boolean validation") {
    def compose(i: Int, s: String): Result[(Int, String), List[ParseErr]] = Validation.validate { v =>
      v.test(i > 0, ParseErr("i must be positive"))
      v.test(s.nonEmpty, ParseErr("s must be non-empty"))
      (i, s)
    }

    assertEquals(compose(1, "ok"), Result.Ok((1, "ok")))
    assertEquals(compose(-1, "ok"), Result.Err(List(ParseErr("i must be positive"))))
    assertEquals(compose(1, ""), Result.Err(List(ParseErr("s must be non-empty"))))
    assertEquals(compose(-1, ""), Result.Err(List(ParseErr("i must be positive"), ParseErr("s must be non-empty"))))
  }


  test("result validation") {

    def compose(i: Int, s: String): Result[(PosInt, NonEmptyStr), List[ParseErr]] = Validation.validate { v =>
      val pI = v.test(PosInt.test(i))
      val nES = v.test(NonEmptyStr.test(s))
      (pI.ok, nES.ok)
    }

    assertEquals(compose(1, "ok"), Result.Ok((PosInt(1), NonEmptyStr("ok"))))
    assertEquals(compose(-1, "ok"), Result.Err(List(ParseErr("i must be positive"))))
    assertEquals(compose(1, ""), Result.Err(List(ParseErr("s must be non-empty"))))
    assertEquals(compose(-1, ""), Result.Err(List(ParseErr("i must be positive"), ParseErr("s must be non-empty"))))
  }

  test("composition of nested scopes") {
    case class Person(name: NonEmptyStr, age: PosInt)
    def validatePerson(name: String, age: Int): Result[Person, List[ParseErr]] = Validation.validate { v =>
      val nameResult = v.test(NonEmptyStr.test(name))
      val ageResult = v.test(PosInt.test(age))
      Person(nameResult.ok, ageResult.ok)
    }
    case class Company(name: NonEmptyStr, ceo: Person)
    def validateCompany(name: String, ceoName: String, ceoAge: Int): Result[Company, List[ParseErr]] = Validation.validate { v =>
      val nameResult = v.test(NonEmptyStr.test(name))
      val ceoResult = v.testAll(validatePerson(ceoName, ceoAge))
      Company(nameResult.ok, ceoResult.ok)
    }
  }

  test("ok with exclusive scopes") {
    type Data = (x: Int, y: String, z: Double)
    val data: Data = (x = 1, y = "ok", z = 3.14)
    assertEquals(
      Validation.validate[Data, ParseErr] { v1 =>
        val inner = v1.testAll(Validation.validate[Data, ParseErr] { v2 =>
          abstracted(data.x, data.y, data.z)(v1, v2)
          data
        })
        inner.ok
      },
      expected = Result.Ok(data)
    )
    assertEquals(
      Validation.validate[Unit, ParseErr] { v1 =>
        val inner = v1.testAll(Validation.validate[Unit, ParseErr] { v2 =>
          abstracted(-1, "", -48.5)(v1, v2)
        })
        inner.ok
      },
      expected = Result.Err(
        List(
          ParseErr("i1 must be positive"),
          ParseErr("s must be non-empty"),
          ParseErr("c must be positive")
        )
      )
    )
    assertEquals(
      Validation.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(v, new Validation)
      },
      expected = Result.Err(List(ParseErr("i1 must be positive")))
    )
    assertEquals(
      Validation.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(new Validation, v)
      },
      expected = Result.Err(List(ParseErr("s must be non-empty"), ParseErr("c must be positive")))
    )
  }

  test("separation failure") {
    // without separation checking then potentially if you assume two error scopes were separate
    // but they were not then composing them could duplicate errors
    assert(
      compileFailed(expr(s"""
      Validation.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(v, v) // error: v duplicated
      }
      """)).exists(_.contains("Separation failure"))
    )
  }

  test("escape") {
    locally {
      // mutable escape not allowed
      assert(
        compileFailed(expr(s"""
        Validation.validate[() => Validation[ParseErr]^, ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Capability `any` outlives its scope"))
      )
    }
    locally {
      // unannotated escape not allowed
      assert(
        compileFailed(expr(s"""
        Validation.validate[() => Validation[ParseErr], ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Capability `any` outlives its scope"))
      )
    }
    locally {
      // pure escape not allowed
      assert(
        compileFailed(expr(s"""
        Validation.validate[() => Validation[ParseErr]^{}, ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Capability `any` outlives its scope"))
      )
    }
    locally {
      // inferred type escape not allowed
      assert(
        compileFailed(expr(s"""
        val escape = Validation.validate { v =>
          () => v
        }
        """)).exists(_.contains("Separation failure"))
      )
    }
    locally {
      // FIXME: WTF!!!!
      assertEquals(
        compileFailed(expr(s"""
        val escape: Result[Validation[Any], List[Any]] = Validation.validate { v =>
          v
        }
        escape match {
          case Result.Ok(v: Validation[Any]^) => v.test(false, "leaked!")
        }
        """)), Nil //.exists(_.contains("Note that steps.result.Validation[Any]^{scope} does not conform"))
      )
    }
    locally {
      // explicit type direct return not allowed
      assert(
        compileFailed(expr(s"""
        val result = Validation.validate[Validation[ParseErr], ParseErr] { v =>
          v
        }
        """)).exists(_.contains("Note that capability `any.rd` cannot flow into capture set {}"))
      )
    }
    locally {
      // boxed inferred return not allowed
      assert(
        compileFailed(expr(s"""
        val result = Validation.validate { v =>
          Escaped(v)
        }
        result.get match {
          case Escaped(v: Validation[Any]) => v.test(false, "leaked!")
        }
        """)).exists(_.contains("Cannot call update method appendOne of Validation_this\n" +
          "since the capture set {} of value v of its prefix (v : steps.result.Validation[Any]^{}) is read-only."))
      )
    }
  }

  def compileFailed(src: String): Seq[String] =
    TestDriver.runWithErrors(src)
  }

object ValidationTest {
  case class ParseErr(msg: String)
  case class Escaped(any: Any)

  def abstracted(a: Int, b: String, c: Double)(v1: Validation[ParseErr]^, v2: Validation[ParseErr]^): Unit =
    v1.test(a > 0, ParseErr("i1 must be positive"))
    v2.test(b.nonEmpty, ParseErr("s must be non-empty"))
    v2.test(c > 0, ParseErr("c must be positive"))
}
