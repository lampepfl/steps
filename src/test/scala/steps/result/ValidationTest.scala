package steps.testing

import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import steps.result.Validation
import ValidationTest.abstracted

val imports = """
import language.experimental.{separationChecking, captureChecking}
import steps.result.Result
import steps.result.Validation
import ValidationTest.abstracted
"""

class ValidationTest extends munit.FunSuite {
  import opaques.*

  object opaques:
    opaque type PosInt = Int
    object PosInt:
      def test(i: Int): Result[PosInt, String] =
        if i > 0 then Result.Ok(i)
        else Result.Err("i must be positive")

      inline def apply(i: Int): PosInt = inline i match
        case _: scala.compiletime.ops.int.S[?] => i

    opaque type NonEmptyStr = String
    object NonEmptyStr:
      def test(s: String): Result[NonEmptyStr, String] =
        if s.nonEmpty then Result.Ok(s)
        else Result.Err("s must be non-empty")

      inline def apply(s: String): NonEmptyStr = inline compiletime.constValue[s.type] match
        case _: "" => compiletime.error("s must be non-empty")
        case _ => s

  test("boolean validation") {
    def compose(i: Int, s: String): Result[(Int, String), List[String]] = Validation.validate { v =>
      v.test(i > 0, "i must be positive")
      v.test(s.nonEmpty, "s must be non-empty")
      (i, s)
    }

    assertEquals(compose(1, "ok"), Result.Ok((1, "ok")))
    assertEquals(compose(-1, "ok"), Result.Err(List("i must be positive")))
    assertEquals(compose(1, ""), Result.Err(List("s must be non-empty")))
    assertEquals(compose(-1, ""), Result.Err(List("i must be positive", "s must be non-empty")))
  }


  test("result validation") {

    def compose(i: Int, s: String): Result[(PosInt, NonEmptyStr), List[String]] = Validation.validate { v =>
      val pI = v.test(PosInt.test(i))
      val nES = v.test(NonEmptyStr.test(s))
      (pI.ok, nES.ok)
    }

    assertEquals(compose(1, "ok"), Result.Ok((PosInt(1), NonEmptyStr("ok"))))
    assertEquals(compose(-1, "ok"), Result.Err(List("i must be positive")))
    assertEquals(compose(1, ""), Result.Err(List("s must be non-empty")))
    assertEquals(compose(-1, ""), Result.Err(List("i must be positive", "s must be non-empty")))
  }

  test("ok with exclusive scopes") {
    val pair = (x = 1, y = "ok")
    assertEquals(
      Validation.validate[(Int, String), String] { v1 =>
        val inner = v1.test(Validation.validate[(Int, String), String] { v2 =>
          abstracted(pair.x, pair.y)(v1, v2)
          pair
        }.mapErr(_.mkString(", ")))
        inner.ok
      },
      expected = Result.Ok(x -> y)
    )
    assertEquals(
      Validation.validate[Unit, String] { v1 =>
        val inner = v1.test(Validation.validate[Unit, String] { v2 =>
          abstracted(-1, "")(v1, v2)
        }.mapErr(_.mkString(", ")))
        inner.ok
      },
      expected = Result.Err(List("i1 must be positive", "s must be non-empty"))
    )
    assertEquals(
      Validation.validate[Unit, String] { v =>
        abstracted(-1, "")(v, new Validation)
      },
      expected = Result.Err(List("i1 must be positive"))
    )
    assertEquals(
      Validation.validate[Unit, String] { v =>
        abstracted(-1, "")(new Validation, v)
      },
      expected = Result.Err(List("s must be non-empty"))
    )
  }

  test("separation failure") {
    assert(
      compileFailed(s"""
      $imports
      def Test =
        Validation.validate[Unit, String] { v =>
          abstracted(-1, "")(v, v) // error: v duplicated
        }
      """).exists(_.contains("Separation failure"))
    )
  }

  def compileFailed(src: String): Seq[String] =
    TestDriver.runWithErrors(src)
  }

object ValidationTest {
  def abstracted(a: Int, b: String)(v1: Validation[String]^, v2: Validation[String]^): Unit =
    v1.test(a > 0, "i1 must be positive")
    v2.test(b.nonEmpty, "s must be non-empty")
}
