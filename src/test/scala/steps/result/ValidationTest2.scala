package steps.testing

import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import Result.eval.{ok}
import steps.result.ValidationScope
import ValidationTest2.ParseErr
import ValidationTest2.Escaped
import ValidationTest2.abstracted

private val prelude2 = """
package steps.testing
import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import Result.eval.{ok}
import steps.result.ValidationScope
import caps.any
import caps.fresh
import ValidationTest2.ParseErr
import ValidationTest2.Escaped
import ValidationTest2.abstracted

"""

def expr2(expr: String) = s"""
$prelude2

def Test = {
  val _ = {${expr.linesWithSeparators.mkString("    ")}
  }
}
"""

class ValidationTest2 extends munit.FunSuite {
  import opaques.*

  test("can append to scope after leak") {
    // def sample1() = {
    //   val escape = ValidationScope.validate[ValidationScope.WritePerm, Any] { v =>
    //     summon[ValidationScope.WritePerm]
    //   }
    //   escape match {
    //     case Result.Ok((v: ValidationScope[Any]^, given ValidationScope.WritePerm)) =>
    //       v.test(false, "leaked!")
    //     case Result.Err(errs) =>
    //       fail(s"unexpected error: $errs")
    //   }
    // }
    def sample0() = {
      val escape = ValidationScope.file { f =>
        f
      }
      escape match {
        case Result.Ok(f: ValidationScope.FileHandle) =>
          println(s"file is closed: ${f.isClosed}")
        case Result.Err(errs) =>
          fail(s"unexpected error: $errs")
      }
    }
    // def sample2() = {
    //   val escape = ValidationScope.validate { v =>
    //     new Escaped {
    //       def any: Any = (v, summon[ValidationScope.WritePerm])
    //     }
    //   }
    //   escape match {
    //     case Result.Ok(v: Escaped) =>
    //       val vScopePair = v.any.asInstanceOf[(ValidationScope[Any]^, ValidationScope.WritePerm)]
    //       val vScope = vScopePair(0)
    //       given ValidationScope.WritePerm = vScopePair(1)
    //       vScope.test(false, "leaked!")
    //     case Result.Err(errs) =>
    //       fail(s"unexpected error: $errs")
    //   }
    // }
    // sample1()
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
    def compose(i: Int, s: String): Result[(Int, String), List[ParseErr]] = ValidationScope.validate { v =>
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

    def compose(i: Int, s: String): Result[(PosInt, NonEmptyStr), List[ParseErr]] = ValidationScope.validate { v =>
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
    def validatePerson(name: String, age: Int): Result[Person, List[ParseErr]] = ValidationScope.validate { v =>
      val nameResult = v.test(NonEmptyStr.test(name))
      val ageResult = v.test(PosInt.test(age))
      Person(nameResult.ok, ageResult.ok)
    }
    case class Company(name: NonEmptyStr, ceo: Person)
    def validateCompany(name: String, ceoName: String, ceoAge: Int): Result[Company, List[ParseErr]] = ValidationScope.validate { v =>
      val nameResult = v.test(NonEmptyStr.test(name))
      val ceoResult = v.testAll(validatePerson(ceoName, ceoAge))
      Company(nameResult.ok, ceoResult.ok)
    }
  }

  test("ok with exclusive scopes") {
    type Data = (x: Int, y: String, z: Double)
    val data: Data = (x = 1, y = "ok", z = 3.14)
    assertEquals(
      ValidationScope.validate[Data, ParseErr] { v1 =>
        val inner = v1.testAll(ValidationScope.validate[Data, ParseErr] { v2 =>
          abstracted(data.x, data.y, data.z)(v1, v2)
          data
        })
        inner.ok
      },
      expected = Result.Ok(data)
    )
    assertEquals(
      ValidationScope.validate[Unit, ParseErr] { v1 =>
        val inner = v1.testAll(ValidationScope.validate[Unit, ParseErr] { v2 =>
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
      ValidationScope.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(v, new ValidationScope)
      },
      expected = Result.Err(List(ParseErr("i1 must be positive")))
    )
    assertEquals(
      ValidationScope.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(new ValidationScope, v)
      },
      expected = Result.Err(List(ParseErr("s must be non-empty"), ParseErr("c must be positive")))
    )
  }

  test("separation failure") {
    // without separation checking then potentially if you assume two error scopes were separate
    // but they were not then composing them could duplicate errors
    assert(
      compileFailed(expr2(s"""
      ValidationScope.validate[Unit, ParseErr] { v =>
        abstracted(-1, "", -48.5)(v, v) // error: v duplicated
      }
      """)).exists(_.contains("Separation failure"))
    )
  }

  test("escape") {
    locally {
      // mutable escape not allowed
      assert(
        compileFailed(expr2(s"""
        ValidationScope.validate[() => ValidationScope[ParseErr]^, ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Separation failure"))
      )
    }
    locally {
      // unannotated escape not allowed
      assert(
        compileFailed(expr2(s"""
        ValidationScope.validate[() => ValidationScope[ParseErr], ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Separation failure"))
      )
    }
    locally {
      // pure escape not allowed
      assert(
        compileFailed(expr2(s"""
        ValidationScope.validate[() => ValidationScope[ParseErr]^{}, ParseErr] { v =>
          () => v
        }
        """)).exists(_.contains("Note that capability `scope.rd` cannot flow into capture set {}"))
      )
    }
    locally {
      // inferred type escape not allowed
      assert(
        compileFailed(expr2(s"""
        ValidationScope.validate { v =>
          () => v
        }
        """)).exists(_.contains("Separation failure"))
      )
    }
  }

  def compileFailed(src: String): Seq[String] =
    TestDriver.runWithErrors(src)
  }

object ValidationTest2 {
  case class ParseErr(msg: String)
  trait Escaped {
    def any: Any
  }

  def abstracted(a: Int, b: String, c: Double)(v1: ValidationScope[ParseErr]^, v2: ValidationScope[ParseErr]^)(using ValidationScope.WritePerm): Unit =
    v1.test(a > 0, ParseErr("i1 must be positive"))
    v2.test(b.nonEmpty, ParseErr("s must be non-empty"))
    v2.test(c > 0, ParseErr("c must be positive"))
}
