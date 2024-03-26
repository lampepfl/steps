import result.Result
import result.Result.*
import scala.util.Try

class ResultTest extends munit.FunSuite {
  type Rs = Result[Int, String]
  val ok: Rs = Ok(1)
  val err: Rs = Err("bad")

  test("disambiguators") {
    assert(ok.isOk)
    assert(!err.isOk)
    assert(!ok.isErr)
    assert(err.isErr)

    assert(ok.forall(_ == 1))
    assert(!ok.forall(_ == 2))
    assert(ok.exists(_ == 1))
    assert(!ok.exists(_ == 2))
    assert(!err.forall(_ == 1))
    assert(!err.exists(_ == 1))

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
    assertEquals(ok.ok, Some(1))
    assertEquals(err.ok, None)
    assertEquals(ok.err, None)
    assertEquals(err.err, Some("bad"))
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
    given [T]: Conversion[T, T] with
      def apply(u: T) = u

    assertEquals(Result.catchException(1), Ok(1))
    val exc = new Exception("bleh")
    assertEquals(Result.catchException(throw exc), Err(exc))

    case object NoLog extends Exception("no log")

    enum LogErr:
      case NL(inner: NoLog.type)

    given Conversion[NoLog.type, LogErr] = LogErr.NL(_)

    def log2(input: Int): Result[Int, LogErr] =
      Result:
        if input < 1 then Result.raise(NoLog)
        else if input == 1 then 0
        else log2(input / 2).? + 1

    assertEquals(log2(4), Ok(2))
    assertEquals(log2(-1), Err(LogErr.NL(NoLog)))
  }
}
