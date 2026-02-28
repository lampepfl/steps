import scala.util.Try

import language.experimental.captureChecking

import steps.result.Result
import steps.result.Result.{eval as _, *}
// import steps.result.Result.eval.*

import steps.result.Result.eval2.*

class ResultTest extends munit.FunSuite {

  val ok: Result[Int, String] = Ok(1)
  val err: Result[Int, String] = Err("bad")

  test("disambiguators") {
    assert(ok.isOk)
    assert(!err.isOk)
    assert(!ok.isErr)
    assert(err.isErr)

    assert(ok.forall(_ == 1))
    assert(!ok.forall(_ == 2))
    assert(ok.isErrOr(_ == 1))
    assert(!ok.isErrOr(_ == 2))
    assert(ok.exists(_ == 1))
    assert(!ok.exists(_ == 2))
    assert(ok.isOkAnd(_ == 1))
    assert(!ok.isOkAnd(_ == 2))
    assert(err.forall(_ == 1) /* forall is always true for Err */)
    assert(err.isErrOr(_ == 1) /* isErrOr is always true for Err */)
    assert(!err.exists(_ == 1))
    assert(!err.isOkAnd(_ == 1))

    assertEquals(ok.get, 1)
    assert(Try(err.get).isFailure)

    assertEquals(err.getErr, "bad")
    assert(Try(ok.getErr).isFailure)

    var tapped = 0
    val tapper = [X] => (_: X) => tapped += 1
    ok.tap(tapper[Int])
    assertEquals(tapped, 1)
    err.tap(tapper[Int])
    assertEquals(tapped, 1)
    ok.tapErr(tapper[String])
    assertEquals(tapped, 1)
    err.tapErr(tapper[String])
    assertEquals(tapped, 2)

    assertEquals(ok.getOrElse { tapper(1); 2 }, 1)
    assertEquals(tapped, 2)
    assertEquals(err.getOrElse { tapper(1); 2 }, 2)
    assertEquals(tapped, 3)
  }

  test("transformers") {
    assertEquals(ok.flatMap(v => Ok(v + 1)), Ok(2))
    assertEquals(ok.flatMap(v => Err("pew")), Err("pew"))
    assertEquals(err.flatMap(v => Ok(v + 1)), err)

    assertEquals(ok.handleErr(_ => err), ok)
    assertEquals(err.handleErr(_ => ok), ok)

    val sum = for
      a <- ok
      b <- ok
    yield a + b
    assertEquals(sum, Ok(2))

    val sumNone = for
      a <- ok
      b <- err
      c <- ok
    yield a + b + c
    assertEquals(sumNone, err)

    assertEquals(Ok(Ok(3)).flatten, Ok(3))
    assertEquals(Ok(Err("bad")).flatten, Err("bad"))
    assertEquals(Err("bad").flatten, Err("bad"))

    assertEquals(ok.map(_ + 1), Ok(2))
    assertEquals(err.map(_ + 1), err)
    assertEquals(ok.mapErr(_ + "!"), ok)
    assertEquals(err.mapErr(_ + "!"), Err("bad!"))
  }

  test("combinators") {
    var tapped = 0

    assertEquals(ok.and(err), Err("bad"))
    assertEquals(err.and(ok), Err("bad"))
    assertEquals(ok.and(ok), Ok((1, 1)))
    assertEquals(err.and { tapped += 1; Err("other") }, Err("bad"))
    assertEquals(tapped, 0)
    assertEquals(ok.and { tapped += 1; Err("other") }, Err("other"))
    assertEquals(tapped, 1)

    tapped = 0
    assertEquals(ok.andTrace(err), Err(Right("bad")))
    assertEquals(err.andTrace(ok), Err(Left("bad")))
    assertEquals(ok.and(ok), Ok((1, 1)))
    assertEquals(err.andTrace { tapped += 1; Err("other") }, Err(Left("bad")))
    assertEquals(tapped, 0)
    assertEquals(ok.andTrace { tapped += 1; Err("other") }, Err(Right("other")))
    assertEquals(tapped, 1)

    assertEquals(err.zip(ok), Err(List("bad")))
    assertEquals(ok.zip(err), Err(List("bad")))
    assertEquals(ok.zip(Ok(2)), Ok((1, 2)))
    assertEquals(err.zip(Err("other")), Err(List("bad", "other")))

    assertEquals(ok cons Result.empty, Ok(1 *: EmptyTuple))
    assertEquals(err cons Result.empty, Err(List("bad")))

    tapped = 0
    assertEquals(ok.or(err), ok)
    assertEquals(err.or(ok), ok)
    assertEquals(err.or(err), err)
    assertEquals(ok.or { tapped += 1; err }, ok)
    assertEquals(tapped, 0)
    assertEquals(err.or { tapped += 1; Err("other") }, Err("other"))
    assertEquals(tapped, 1)
  }

  test("conversions") {
    assertEquals(ok.toOption, Some(1))
    assertEquals(err.toOption, None)
    assertEquals(ok.errOption, None)
    assertEquals(err.errOption, Some("bad"))
    assertEquals(ok.toOption, Some(1))
    assertEquals(err.toOption, None)

    assertEquals(ok.toEither, Right(1))
    assertEquals(err.toEither, Left("bad"))

    assertEquals(ok.toSeq, Seq(1))
    assertEquals(err.toSeq, Seq())

    assertEquals(Ok(1).toTry.get, 1)
    val exc = new Exception("bleh")
    assertEquals(Err(exc).toTry, scala.util.Failure(exc))
  }

  test("constructors") {
    assertEquals(Result.catchException({ case ex => ex })(1), Ok(1))
    val exc = new Exception("bleh")
    assertEquals(
      Result.catchException({ case ex: Exception => ex })(throw exc),
      Err(exc)
    )

    case object NoLog extends Exception("no log")

    assert(
      Try { Result.catchException({ case NoLog => 1 })(throw exc) }.isFailure
    )

    enum LogErr:
      case NL(inner: NoLog.type)

    given Conversion[NoLog.type, LogErr] = LogErr.NL(_)

    given Conversion[LogErr, Exception] =
      case LogErr.NL(inner) => inner

    def log2(input: Int): Result[Int, LogErr] =
      Result:
        if input < 1 then eval2.raise(NoLog)
        else if input == 1 then 0
        else log2(input / 2).ok + 1

    Result[Int, Exception]: label ?=>
      Result.resultEvaluator.ok(log2(5))//.ok

    assertEquals(log2(4), Ok(2))
    assertEquals(log2(-1), Err(LogErr.NL(NoLog)))
  }

  class MyException(val msg: String) extends Exception(msg):
    override def equals(obj: Any): Boolean = obj match
      case ex: MyException => msg == ex.msg
      case _                  => false
  test("capture throwing") {
    class Logger:
      var log: List[String] = Nil
      def logMsg(msg: String): Unit = log = msg :: log

    def exec(using log: Logger^): () ->{log} Result[Any, Exception] =
      () => Result.catchException({ case ex: Exception => log.logMsg(s"caught ${ex}"); ex }) {
        throw new MyException("oops")
      }

    val logger = new Logger
    val result = exec(using logger)()
    assertEquals(result, Err(MyException("oops")))
  }

  test("tailrec") {
    given [T]: Conversion[T, T] with
      def apply(u: T) = u

    @scala.annotation.tailrec
    def tryFoldLeft[T, U, E](init: U, f: (U, T) => Result[U, E])(
        s: Seq[T]
    ): Result[U, E] =
      Result:
        s match
          case Seq() => init
          case Seq(h, t*) =>
            val next = f(init, h).ok
            eval2.break(tryFoldLeft(next, f)(t))
  }

  test("implicit upcasting") {
    import steps.result.Conversions.Lift.given

    Result[Int, Nothing]:
      val t: Result[Int, String] = 1
      eval2.break(1)
  }

  test("iterable") {
    assertEquals(ok.iterator.toSeq, ok.toSeq)
    assertEquals(err.iterator.toSeq, err.toSeq)

    assertEquals(ok.knownSize, 1)
    assertEquals(err.knownSize, 0)
  }

  // using old design where .ok can be passed explicit label in postfix position
  // test("scoping") {
  //   given Conversion[Int, Bar] = Bar(_)
  //   given Conversion[Foo, Bar] = f => Bar(f.x)
  //   class Bar(val x: Int)
  //   class Foo(val x: Int)
  //   Result[Int, Bar]: l1 ?=>
  //     Result[String, Foo]: l2 ?=>
  //       val r: Result[String, Int] = Result.Err(1)
  //       val str: String =  r.ok(using l1)
  //       str
  //     .ok.length
  // }

  test("scoping 2") {
    given Conversion[Int, Bar] = Bar(_)
    given Conversion[Foo, Bar] = f => Bar(f.x)
    class Bar(val x: Int)
    class Foo(val x: Int)
    Result[Int, Bar]: l1 ?=>
      Result[String, Foo]: l2 ?=>
        val r: Result[String, Int] = Result.Err(1)
        val str: String = eval2.ok(using l1)(r)
        str
      .ok.length
  }

  // Error message tests, uncomment to see.

  test("break error") {
    given Conversion[String, Int] = _.length // comment for failure
    Result[Int, Int]:
      val z = eval2.break(err)
      1
  }

  // test("outside of scope") {
  //   val x = ok.ok
  // }

  // test("wrong error type") {
  //   Result[Int, Int]:
  //     val x = ok.ok
  //     val y = eval.raise("a")
  //     1
  // }

  // test("leakage") {
  //   Result[util.boundary.Label[?], Any]: (label: util.boundary.Label[?]) ?=>
  //     label
  // }
}
