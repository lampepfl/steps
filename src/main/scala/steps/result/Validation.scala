package steps.result

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label}
import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import Validation.{Validated, Checked}

object Validation {
  object Abort
  type Abort = Abort.type

  type Checked[+T] = Label[Abort] ?=> T

  inline def validate[T, E](inline op: Validation[E]^ => Checked[T]): Result[T, List[E]] =
    val scope: Validation[E]^ = new Validation[E]
    val userResult =
      boundary[Validated[T]]:
        Validated.success(op(scope))
    caps.freeze(scope)
    val errors = scope.snapshot
    userResult match
      case ok: Result.Ok[?] if errors.isEmpty => ok
      case _ => Result.Err(errors)


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

  update def appendOne(e: E): Unit =
    errors += e

  update inline def test(inline cond: Boolean, inline error: E): Unit =
    if !cond then
      appendOne(error)

  update inline def test[A](inline cond: Result[A, E]): Validated[A] =
    cond match
      case ok: Result.Ok[?] =>
        Validated.fromOk(ok)
      case Result.Err(e) =>
        appendOne(e)
        Validated.failure

  update inline def require(inline cond: Boolean, inline error: E): Checked[Unit] =
    if !cond then
      appendOne(error)
      break(Validation.Abort)

  update inline def require[A](inline cond: Result[A, E]): Checked[A] =
    cond match
      case ok: Result.Ok[?] =>
        ok.value
      case Result.Err(e) =>
        appendOne(e)
        break(Validation.Abort)
