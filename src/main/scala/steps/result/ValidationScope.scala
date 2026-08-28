package steps.result

import language.experimental.{captureChecking, separationChecking}

import scala.util.boundary, boundary.{break, Label}

import collection.mutable
import caps.Control
import caps.fresh
import caps.any
import caps.Control

import ValidationScope.{Checked, WritePerm}
import scala.annotation.publicInBinary

object ValidationScope {

  sealed trait WritePerm extends caps.SharedCapability

  type Checked[+T] = boundary.Label[Result[Nothing, Unit]] ?=> T

  @publicInBinary
  private[result] def unwrap[T, E](
      consume scope: ValidationScope[E]^,
      userResult: Result[T, Unit]^{}
  ): Result[T, List[E]] =
    val errs = scope.clear
    userResult match
      case ok @ Result.Ok(_) if errs.isEmpty => ok
      case _ => Result.Err(errs)

  inline def validate[T, E](inline body: (boundary.Label[Result[Nothing, Unit]], WritePerm) ?=> ValidationScope[E]^ => T): Result[T, List[E]] =
    // FIXME: It seems impossible to drop the inline requirement, or lift out the
    // implementation here, without also requiring explicit type parameters at the call site.
    val scope: ValidationScope[E]^ = new ValidationScope[E]
    locally {
      // given WritePerm = WritePerm
      val userResult = Result[T, Unit]: lbl ?=>
        body(using lbl, null.asInstanceOf[WritePerm])(scope)
      ValidationScope.unwrap(scope, userResult)
    }

  class FileHandle extends caps.Stateful, caps.SharedCapability {
    var isClosed = false
    update def close(): Unit = {
      isClosed = true
    }
  }
  def file[T](body: FileHandle^ => T): Result[T, Nothing] =
    val fileHandle = new FileHandle
    try {
      Result.Ok(body(fileHandle))
    } finally {
      fileHandle.close()
    }

}

class ValidationScope[E] extends caps.Mutable:
  self: ValidationScope[E]^{any} =>

  private val errors = mutable.ListBuffer[E]()

  @publicInBinary
  private[ValidationScope] def clear: List[E] = {
    val es = errors.toList
    errors.clear()
    es
  }

  @publicInBinary
  private[ValidationScope] update def appendOne(e: E)(using WritePerm): Unit =
    errors += e

  @publicInBinary
  private[ValidationScope] update def appendAll(es: List[E])(using WritePerm): Unit =
    errors ++= es

  update inline def test(cond: Boolean, inline error: E)(using WritePerm): Unit =
    if !cond then
      appendOne(error)

  update inline def require(cond: Boolean, inline error: E)(using WritePerm): Checked[Unit] =
    if !cond then
      appendOne(error)
      break(Result.invalid)

  update def test[A](cond: Result[A, E])(using WritePerm): Result[A, Unit] =
    cond match
      case ok: Result.Ok[?] =>
        ok
      case Result.Err(e) =>
        appendOne(e)
        Result.invalid

  update def testAll[A](cond: Result[A, List[E]])(using WritePerm): Result[A, Unit] =
    cond match
      case ok: Result.Ok[?] =>
        ok
      case Result.Err(es) =>
        appendAll(es)
        Result.invalid
