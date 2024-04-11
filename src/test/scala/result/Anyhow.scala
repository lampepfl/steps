import dotty.result.Result
import dotty.result.Result.*
import dotty.result.Result.eval.*
import scala.util.Try
import scala.util.boundary

/** Just something to test. We don't encourage putting Throwables inside results
  * everywhere (just throw!!)
  */
class AnyhowClone extends munit.FunSuite {
  case class AnyErr private[AnyhowClone] (
      error: Any,
      stackTrace: Array[StackTraceElement]
  ) extends java.lang.Throwable(error.toString()):
    setStackTrace(stackTrace)

    override def toString() = s"Anyhow error: $error"

  given Conversion[Any, AnyErr] =
    case anyErr: AnyErr => anyErr
    case any =>
      new AnyErr(
        any,
        Thread.currentThread().getStackTrace().drop(5) /* 5 conversion frames */
      )

  type Anyhow[+T] = Result[T, AnyErr]

  object Anyhow:
    export Result.{apply as _, *}

    inline def apply[T](
        inline body: boundary.Label[Anyhow[T]] ?=> T
    ): Anyhow[T] =
      Result.apply[T, AnyErr](body)

    extension [T](a: Anyhow[T])
      def unwrap =
        a match
          case Ok(value)  => value
          case Err(error) => throw error

  test("anyhow") {
    import Anyhow.*

    val err = Anyhow:
      eval.raise("noooo")

    @scala.annotation.tailrec
    def loop(n: Int)(cur: Int): Anyhow[Int] =
      Anyhow[Int]:
        if n == cur then eval.raise(s"noooo $cur")
        else eval.break(loop(n)(cur + 1))

    loop(10)(0)
    err
  }
}
