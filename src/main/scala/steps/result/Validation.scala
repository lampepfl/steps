package steps.result

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label, Break}

import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import Validation.{Checked, Validated, Tested, CanCheck, boundary2}
import scala.annotation.publicInBinary
import steps.result.Validation.Step

object Validation {

  type Validated[+T, E] = Result[T, List[E]]
  type Tested[+T] = Result[T, Unit]
  type CanCheck = boundary.Label[Result.Err[Unit]]
  type Checked[+T] = CanCheck ?=> T
  type Step[+T, E] = (CanCheck, Validation[E]^) ?=> T
  def scope[E](using scope: Validation[E]^): scope.type = scope

  inline def validated[T, E](inline step: Step[T, E]): Validated[T, E] =
    Validation[E].result(step)

  /** Version of boundary that allows for an exclusive label from the body - still gets optimised */
  inline def boundary2[A, E](inline body: Label[E] ?=> A): A | E =
    val local = Label[E]()
    try body(using local)
    catch case ex: Break[E] @unchecked =>
      if ex.isSameLabelAs(local) then ex.value
      else throw ex

}

class Validation[E] extends caps.Stateful, caps.ExclusiveCapability:
  self: Validation[E]^{any} =>

  private val errors = mutable.ListBuffer[E]()

  @publicInBinary
  private[Validation] consume def close(): List[E] = {
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

  update inline def require(cond: Boolean, inline error: E): Checked[Unit] = (lbl: CanCheck) ?=>
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

  inline update def testStep[A](inline cond: Step[A, E]): Tested[A] =
    given scope: (Validation[E]^{this}) = this
    boundary2 {
      Result.Ok(cond)
    }

  update def testAll[A](validated: Validated[A, E]): Tested[A] =
    validated match
      case ok: Result.Ok[?] =>
        ok
      case Result.Err(es) =>
        appendAll(es)
        Result.invalid

  inline consume def result[A](consume inline cond: Step[A, E]): Validated[A, E] =
    val validated = testStep(cond)
    finish(validated)

  @publicInBinary
  private[Validation] consume def finish[A](consume validated: Tested[A]): Validated[A, E] =
    val errs = close()
    validated match
      case ok @ Result.Ok(_) if errs.isEmpty => ok
      case _ => Result.Err(errs)
