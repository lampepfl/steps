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
  object Abort
  type Abort = Abort.type

  type Checked[+T] = Label[Abort] ?=> T

  @publicInBinary
  private[Validation] def unwrap[T, E](
      consume scope: Validation[E]^,
      userResult: Validated[T]
  ): Result[T, List[E]] =
    val errors = scope.snapshot
    userResult match
      case ok: Result.Ok[?] if errors.isEmpty => ok
      case _ => Result.Err(errors)

  inline def validate[T, E](inline op: Validation[E]^ => Checked[T]): Result[T, List[E]] =
    // FIXME: It seems impossible to drop the inline requirement, or lift out the
    // implementation here, without also requiring explicit type parameters at the call site.
    val scope: Validation[E]^ = new Validation[E]
    val userResult = boundary[Validated[T]]:
      Validated.success(op(scope))
    Validation.unwrap(scope, userResult)


  opaque type Validated[+A] = Result.Ok[A] | Abort
  object Validated:
    def failure: Validated[Nothing] = Abort
    def success[A](value: A): Validated[A] = Result.Ok(value)
    def fromOk[A](value: Result.Ok[A]): Validated[A] = value
    extension [A](c: Validated[A])
      inline def ok: Checked[A] = c match
        case ok: Result.Ok[?] => ok.value
        case _ => break(Abort)
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
      break(Validation.Abort)

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
