package steps.result

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label}

import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import Validation.{Checked, Validated, Tested}
import scala.annotation.publicInBinary
import scala.annotation.experimental
import steps.result.Validation.Step

object Validation {

  type Validated[+T, E] = Result[T, List[E]]
  type Tested[+T] = Result[T, Unit]
  type CanCheck = boundary.Label[Result.Err[Unit]]
  type Checked[+T] = CanCheck ?=> T
  type Step[+T, E] = (CanCheck, Validation[E]^) ?=> T
  def scope[E](using scope: Validation[E]^): scope.type = scope

  @experimental("We would like to make this inline, but that breaks capture checking, see https://github.com/scala/scala3/issues/26982")
  def validated[T, E](step: Step[T, E]): Validated[T, E] =
    given (Validation[E]^)()
    scope.result(step)

}

class Validation[E] extends caps.Stateful, caps.ExclusiveCapability:
  self: Validation[E]^{any} =>

  private val errors = mutable.ListBuffer[E]()

  consume def close(): List[E] = {
    val es = errors.toList
    errors.clear()
    es
  }

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
      break(Result.invalid)

  update def test[A](cond: Result[A, E]): Tested[A] =
    cond match
      case ok: Result.Ok[?] =>
        ok
      case Result.Err(e) =>
        appendOne(e)
        Result.invalid

  @experimental("We would like to make this inline, but that breaks capture checking, see https://github.com/scala/scala3/issues/26982")
  update def testStep[A](cond: Step[A, E]): Tested[A] =
    boundary: lbl ?=>
      Result.Ok(cond(using lbl, this))

  update def testAll[A](validated: Validated[A, E]): Tested[A] =
    validated match
      case ok: Result.Ok[?] =>
        ok
      case Result.Err(es) =>
        appendAll(es)
        Result.invalid

  @experimental("We would like to make this inline, but that breaks capture checking, see https://github.com/scala/scala3/issues/26982")
  consume def result[A](cond: Step[A, E]): Validated[A, E] =
    val validated = testStep(cond)
    val errs = close()
    validated match
      case ok @ Result.Ok(_) if errs.isEmpty => ok
      case _ => Result.Err(errs)
