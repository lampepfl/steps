import scala.language.implicitConversions
import scala.util.{Try, Success, Failure}

import steps.result.Result
import steps.result.Result.*
import steps.result.ScalaConverters.*

/** Tests conversions from Scala standard library containers into [[Result]].
  */
class FromStdTest extends munit.FunSuite {

  test("okOr") {
    assertEquals(Some(1).okOr("bad"), Ok(1))
    assertEquals((None: Option[Int]).okOr("bad"), Err("bad"))

    var tapped = 0
    assertEquals(Some(1).okOr { tapped += 1; "bad" }, Ok(1))
    assertEquals(tapped, 0 /* the error is never evaluated for Some */ )
  }

  test("asResult on Either") {
    assertEquals((Right(1): Either[String, Int]).asResult, Ok(1))
    assertEquals((Left("bad"): Either[String, Int]).asResult, Err("bad"))
  }

  test("asResult on Try") {
    val exc = new Exception("bleh")
    assertEquals(Success(1).asResult, Ok(1))
    assertEquals((Failure(exc): Try[Int]).asResult, Err(exc))
  }

  test("FromScala conversions") {
    import steps.result.Conversions.FromScala.given

    val fromRight: Result[Int, String] = Right(1): Either[String, Int]
    assertEquals(fromRight, Ok(1))
    val fromLeft: Result[Int, String] = Left("bad"): Either[String, Int]
    assertEquals(fromLeft, Err("bad"))

    val exc = new Exception("bleh")
    val fromSuccess: Result[Int, Throwable] = Success(1): Try[Int]
    assertEquals(fromSuccess, Ok(1))
    val fromFailure: Result[Int, Throwable] = Failure(exc): Try[Int]
    assertEquals(fromFailure, Err(exc))
  }

  test("FromOption conversion") {
    import steps.result.Conversions.FromOption.given

    val fromSome: Result[Int, java.util.NoSuchElementException] = Some(1)
    assertEquals(fromSome, Ok(1))

    val fromNone: Result[Int, java.util.NoSuchElementException] = None
    assert(fromNone.isErr)
  }
}
