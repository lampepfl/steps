package steps.result

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label}
import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import Validation.{Validated, Checked}
import scala.annotation.publicInBinary

object Validation {
  private object AbortImpl
  opaque type Abort = AbortImpl.type

  type Checked[+T] = Label[Validated[Nothing]] ?=> T

  @publicInBinary
  private[Validation] def unwrap[T, E](
      consume scope: Validation[E]^,
      userResult: Validated[T]
  ): Result[T, List[E]] =
    userResult.okOrElse(scope.snapshot)

  inline def validate[T, E](inline op: Validation[E]^ => Checked[T]): Result[T, List[E]] =
    // FIXME: It seems impossible to drop the inline requirement, or lift out the
    // implementation here, without also requiring explicit type parameters at the call site.
    val scope: Validation[E]^ = new Validation[E]
    val userResult = boundary[Validated[T]]:
      Validated.success(op(scope))
    Validation.unwrap(scope, userResult)


  final class Validated[+A](c: Result.Ok[A] | Abort) extends AnyVal {
    private[Validation] def okOrElse[E](errs: List[E]): Result[A, List[E]] = c match
      case ok: Result.Ok[A] if errs.isEmpty => ok
      case _ => Result.Err(errs)
    inline def ok: Checked[A] = c match
      case ok: Result.Ok[?] => ok.value
      case _ => break(this.asInstanceOf[Validated[Nothing]])
  }
  object Validated:
    val failure: Validated[Nothing] = Validated(AbortImpl)
    def success[A](value: A): Validated[A] = Validated(Result.Ok(value))
    def fromOk[A](value: Result.Ok[A]): Validated[A] = Validated(value)
}

class Validation[E] extends caps.Mutable:
  self: Validation[E]^{any} =>

  private val errors = mutable.ListBuffer[E]()

  def snapshot: List[E] = errors.toList

  @publicInBinary
  private[Validation] update def appendOne(e: E): Unit =
    errors += e

  @publicInBinary
  private[Validation] update def appendAll(es: List[E]): Unit =
    errors ++= es

  update inline def test(cond: Boolean, inline error: E): Unit =
    if !cond then
      appendOne(error)

  update inline def require(cond: Boolean, inline error: E): Checked[Unit] =
    if !cond then
      appendOne(error)
      break(Validated.failure)

  update def test[A](cond: Result[A, E]): Validated[A] =
    cond match
      case ok: Result.Ok[?] =>
        Validated.fromOk(ok)
      case Result.Err(e) =>
        appendOne(e)
        Validated.failure

  update def testAll[A](cond: Result[A, List[E]]): Validated[A] =
    cond match
      case ok: Result.Ok[?] =>
        Validated.fromOk(ok)
      case Result.Err(es) =>
        appendAll(es)
        Validated.failure
