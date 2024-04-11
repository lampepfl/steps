package dotty.result
import scala.util.{Try, Success, Failure}

/** Provides extension methods convert Scala API optional value containers into
  * [[Result]].
  *
  * By default, [[Result]] already provides conversions back into these
  * containers through [[Result.ok]], [[Result.toEither]] and [[Result.toTry]].
  */
object ScalaConverters:
  extension [T](op: Option[T])
    /** Converts an [[Option Option[T]]] into a [[Result Result[T, E]]] by
      * providing an error for the [[None]] case.
      */
    def okOr[E](error: => E): Result[T, E] = op match
      case None        => Result.Err(error)
      case Some(value) => Result.Ok(value)

  extension [E, T](either: Either[E, T])
    /** Converts an [[Either Either[E, T]]] into a [[Result Result[T, E]]]. Note
      * that the types are reversed in order.
      */
    def asResult: Result[T, E] = either match
      case Left(value)  => Result.Err(value)
      case Right(value) => Result.Ok(value)

  extension [T](t: Try[T])
    /** Converts a [[scala.util.Try Try[T]]] into a
      * [[Result Result[T, Throwable]]].
      */
    def asResult: Result[T, Throwable] = t match
      case Failure(exception) => Result.Err(exception)
      case Success(value)     => Result.Ok(value)

/** Defines possible implicit conversions into [[Result]] from various types.
  * {{{
  * // for conversions from `Either` and `scala.util.Try`
  * import Conversions.FromScala.given
  * // for conversions from `Option` into `Result[T, ResultIsErrException]`
  * import Conversions.FromOption.given
  * // for implicit lifting from any type `T` into `Result[T, Nothing]`
  * import Conversions.Lift.given
  *
  * }}}
  *
  * Additionally, `import Conversions.Compat.given` provides an instance of
  * `Conversion[T, T]` to aid with using [[Result.eval]] functions.
  *
  * {{{
  * import Conversions.Compat.given
  *
  * val ok: Result[Int, String] = Ok(3)
  * Result[Int, Any]:
  *   ok.? // Ok, Conversion[String, Any] is present
  * }}}
  */
object Conversions:
  import ScalaConverters.*

  object FromScala:
    given [E, T]: Conversion[Either[E, T], Result[T, E]] = _.asResult
    given [T]: Conversion[Try[T], Result[T, Throwable]] = _.asResult

  object FromOption:
    given [T]
        : Conversion[Option[T], Result[T, java.util.NoSuchElementException]] =
      _.okOr(new NoSuchElementException())

  object Lift:
    given [T]: Conversion[T, Result[T, Nothing]] = Result.Ok(_)

  object Compat:
    inline given [T]: Conversion[T, T] = identity
