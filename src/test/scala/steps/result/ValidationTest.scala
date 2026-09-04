package steps.testing

import scala.annotation.experimental
import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import Result.eval.{ok}
import Result.eval
import Result.apply as result
import steps.result.Validation
import steps.result.Validation.scope
import Validation.Validated

def expr2(expr: String) = s"""
package steps.testing

import scala.annotation.experimental
import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import Result.eval.{ok}
import Result.eval
import Result.apply as result
import steps.result.Validation
import steps.result.Validation.scope

@experimental
def Test = {
  import ValidationTest.ParseErr
  import ValidationTest.Escaped
  import ValidationTest.abstracted
  val _ = {${expr.linesWithSeparators.mkString("    ")}
  }
}
"""

@experimental
class ValidationTest extends munit.FunSuite {
  import opaques.*
  import ValidationTest.ParseErr
  import ValidationTest.Escaped
  import ValidationTest.abstracted

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
    def compose(i: Int, s: String): Validated[(Int, String), ParseErr] = Validation.validated {
      scope.test(i > 0, ParseErr("i must be positive"))
      scope.test(s.nonEmpty, ParseErr("s must be non-empty"))
      (i, s)
    }

    assertEquals(compose(1, "ok"), Result.Ok((1, "ok")))
    assertEquals(compose(-1, "ok"), Result.Err(List(ParseErr("i must be positive"))))
    assertEquals(compose(1, ""), Result.Err(List(ParseErr("s must be non-empty"))))
    assertEquals(compose(-1, ""), Result.Err(List(ParseErr("i must be positive"), ParseErr("s must be non-empty"))))
  }


  test("result validation") {

    def compose(i: Int, s: String): Validated[(PosInt, NonEmptyStr), ParseErr] = {
      Validation.validated:
        val pI = scope.test(PosInt.test(i))
        val nES = scope.test(NonEmptyStr.test(s))
        (pI.ok, nES.ok)
    }

    assertEquals(compose(1, "ok"), Result.Ok((PosInt(1), NonEmptyStr("ok"))))
    assertEquals(compose(-1, "ok"), Result.Err(List(ParseErr("i must be positive"))))
    assertEquals(compose(1, ""), Result.Err(List(ParseErr("s must be non-empty"))))
    assertEquals(compose(-1, ""), Result.Err(List(ParseErr("i must be positive"), ParseErr("s must be non-empty"))))
  }

  test("composition of nested scopes") {
    case class Person(name: NonEmptyStr, age: PosInt)

    def validatePerson(name: String, age: Int): Validation.Step[Person, ParseErr] =
      // errors will go to the outer scope, so callers can decide
      // if a nested scope is needed or not. If the errors carry their own tag
      // then maybe nesting isnt needed.
      // or e.g. for form data you only care to report alongside the original field,
      // so a flat scope is fine too.
      val nameResult = scope.test(NonEmptyStr.test(name))
      val ageResult = scope.test(PosInt.test(age))
      Person(nameResult.ok, ageResult.ok)

    case class Company(name: NonEmptyStr, ceo: Person)
    def validateCompany(name: String, ceoName: String, ceoAge: Int): Validated[Company, ParseErr] = {
      Validation.validated:
        val nameResult = scope.test(NonEmptyStr.test(name))

        val ceoResult = scope.test:
          Validation.validated:
            validatePerson(ceoName, ceoAge)
          .mapErr: errs =>
            // in this case compose the errors from a nested scope into a single error
            // for the outer scope
            ParseErr(s"errors in person: [${errs.map(_.msg).mkString(", ")}]")

        Company(nameResult.ok, ceoResult.ok)
    }
  }

  test("ok with exclusive scopes") {
    type Data = (x: Int, y: String, z: Double)
    val data: Data = (x = 1, y = "ok", z = 3.14)
    assertEquals(
      Validation.validated[Data, ParseErr] {
        val v1 = scope
        val inner = v1.testAll(Validation.validated[Data, ParseErr] {
          val v2 = scope
          abstracted(data.x, data.y, data.z)(v1, v2)
          data
        })
        inner.ok
      },
      expected = Result.Ok(data)
    )
    assertEquals(
      Validation.validated[Unit, ParseErr] {
        val v1 = scope
        val inner = v1.testAll(Validation.validated[Unit, ParseErr] {
          val v2 = scope
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
      Validation.validated[Unit, ParseErr] {
        val v = scope
        abstracted(-1, "", -48.5)(v, new Validation)
      },
      expected = Result.Err(List(ParseErr("i1 must be positive")))
    )
    assertEquals(
      Validation.validated[Unit, ParseErr] {
        val v = scope
        abstracted(-1, "", -48.5)(new Validation, v)
      },
      expected = Result.Err(List(ParseErr("s must be non-empty"), ParseErr("c must be positive")))
    )
  }

  test("separation failure") {
    // without separation checking then potentially if you assume two error scopes were separate
    // but they were not then composing them could duplicate errors
    assert(
      compileFailed(s"""
      Validation.validated[Unit, ParseErr] {
        val v = scope
        abstracted(-1, "", -48.5)(v, v) // error: v duplicated
      }
      """).exists(_.contains("Separation failure"))
    )
  }

  test("escape") {
    locally {
      // mutable escape not allowed
      assert(
        compileFailed(s"""
        Validation.validated[() => Validation[ParseErr]^, ParseErr] {
          val v = scope
          () => v
        }
        """).exists(_.contains("Note that capability `v` cannot flow into capture set {any}"))
      )
    }
    locally {
      // unannotated escape not allowed
      assert(
        compileFailed(s"""
        Validation.validated[() => Validation[ParseErr], ParseErr] {
          val v = scope
          () => v
        }
        """).exists(_.contains("Note that capability `v.rd` cannot flow into capture set {any.rd}"))
      )
    }
    locally {
      // pure escape not allowed
      assert(
        compileFailed(s"""
        Validation.validated[() => Validation[ParseErr]^{}, ParseErr] {
          val v = scope
          () => v
        }
        """).exists(_.contains("Note that capability `v.rd` cannot flow into capture set {}."))
      )
    }
    locally {
      // inferred type escape not allowed
      assert(
        compileFailed(s"""
        Validation.validated {
          val v = scope
          () => v
        }
        """).exists(_.contains("Capability `contextual$2` outlives its scope"))
      )
    }
  }

  def compileFailed(src: String): Seq[String] =
    TestDriver.runWithErrors(expr2(src))
  }

object ValidationTest {
  case class ParseErr(msg: String)
  trait Escaped {
    def any: Any
  }

  def abstracted(a: Int, b: String, c: Double)(v1: Validation[ParseErr]^, v2: Validation[ParseErr]^): Unit =
    v1.test(a > 0, ParseErr("i1 must be positive"))
    v2.test(b.nonEmpty, ParseErr("s must be non-empty"))
    v2.test(c > 0, ParseErr("c must be positive"))
}
