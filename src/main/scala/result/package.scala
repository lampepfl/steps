/** Defines [[Result]], an enum representing either a success value or an error.
  */
package result

import java.{util => ju}
import scala.util.control.NonFatal
import scala.util.boundary

export Result.*

/** Represents either a success value of type `T` or an error of type `E`.
  * @groupname construct Constructing [[Result Results]] through other means
  * @groupprio construct 1
  * @groupname eval Constructing [[Result Results]] through evaluating
  * @groupprio eval 0
  * @groupname convert Conversions into other types
  * @groupprio convert 5
  * @groupname transform Transformers to different types under [[Result]]
  * @groupprio transform 3
  * @groupname combine Combining multiple results into a new one
  * @groupprio combine 4
  * @groupname access Value accessors and disambiguators
  * @groupprio access 2
  */
enum Result[+T, +E]:
  /** Contains the success value */
  case Ok[+T](value: T) extends Result[T, Nothing]

  /** Contains the error value */
  case Err[+E](error: E) extends Result[Nothing, E]
end Result

/** @groupname construct Constructing [[Result Results]] through other means
  * @groupprio construct 1
  * @groupname eval Constructing [[Result Results]] through evaluating
  * @groupprio eval 0
  * @groupname convert Extensions: conversions into other types
  * @groupprio convert 5
  * @groupname transform Extensions: transformers to different types under the result
  * @groupprio transform 3
  * @groupname combine Extensions: combining multiple results into a new one
  * @groupprio combine 4
  * @groupname access Extensions: value accessors and disambiguators
  * @groupprio access 2
  */
object Result:
  /** Method implementations on [[Result]]. */
  extension [T, E](r: Result[T, E])

    // Basic case differentiation

    /** Returns whether the result is [[Result.Ok Ok]].
      * @group access
      */
    def isOk: Boolean = r.isInstanceOf[Ok[_]]

    /** Returns whether the result is [[Result.Err Err]].
      * @group access
      */
    def isErr: Boolean = r.isInstanceOf[Err[_]]

    /** Returns `true` if result contains an [[Ok]] value that satisfies `pred`.
      * @group access
      */
    def forall(pred: T => Boolean): Boolean = r match
      case Ok(value) => pred(value)
      case _         => false

    /** Returns `true` if result contains an [[Ok]] value that satisfies `pred`.
      * @group access
      */
    def exists(pred: T => Boolean): Boolean = forall(pred)

    // Conversion to Option and other Seqs

    /** Converts the result into an [[Option]], with [[Some]] case if the value
      * is [[Ok]].
      * @group convert
      */
    def toOption: Option[T] = r match
      case Ok(value) => Some(value)
      case Err(_)    => None

    /** Returns the [[Ok]] value from the result. An alias of [[toOption]].
      * @group convert
      */
    inline def ok: Option[T] = toOption

    /** Returns the [[Err]] error from the result.
      * @group convert
      */
    def err: Option[E] = r match
      case Ok(_)      => None
      case Err(error) => Some(error)

    /** Returns the [[Seq]] that is one element (the [[Ok]] value) or otherwise
      * empty.
      * @group convert
      */
    def toSeq: Seq[T] = r match
      case Ok(value) => Seq(value)
      case _         => Seq()

    // Conversion to Either

    /** Converts the result into an [[Either]].
      * @group convert
      */
    def toEither = r match
      case Ok(value)  => Right(value)
      case Err(error) => Left(error)

    // Extracting values forcefully

    /** Returns the [[Ok]] value from the result, and throws an exception if it
      * is an error.
      * @throws java.util.NoSuchElementException
      *   if the result is an [[Err]].
      * @group access
      */
    def get: T = r match
      case Ok(value) => value
      case Err(_) =>
        throw ju.NoSuchElementException(s"Expected Result.Ok, got $r")

    /** Returns the error from the result, and throws an exception if it is an
      * [[Ok]] value.
      * @throws java.util.NoSuchElementException
      *   if the result is an [[Ok]].
      * @group access
      */
    def getErr: E = r match
      case Err(error) => error
      case Ok(_) =>
        throw ju.NoSuchElementException(s"Expected Result.Err, got $r")

    // Extracting with defaults

    /** Returns the [[Ok]] value from the result, or `default` if the result is
      * an [[Error]].
      * @group access
      */
    def getOrElse(default: => T): T = r match
      case Ok(value) => value
      case Err(_)    => default

    // Tapping

    /** Runs `peek` with the wrapped [[Ok]] value, if it exists.
      * @group access
      */
    def tap(peek: T => Unit): r.type =
      r match
        case Ok(value) => peek(value)
        case Err(_)    => ()
      r

    /** Runs `peek` with the wrapped [[Err]] error, if it exists.
      * @group access
      */
    def tapErr(peek: E => Unit): r.type =
      r match
        case Ok(_)      => ()
        case Err(error) => peek(error)
      r

    // Combinators

    /** Returns a tuple of `r` and `other` if both are [[Ok]], or the first
      * error otherwise. Short-circuits, so `other` is not evaluated if `r` is
      * an [[Err]].
      * @group combine
      */
    def and[U, E1](other: => Result[U, E1]) = r match
      case Err(error) => Err(error)
      case Ok(t) =>
        other match
          case Ok(u)      => Ok((t, u))
          case Err(error) => Err(error)

    /** Returns a tuple of `r` and `other` if both are [[Ok]], or the first
      * error otherwise. Short-circuits, so `other` is not evaluated if `r` is
      * an [[Err]].
      *
      * Unlike [[and]], returns the error disambiguated by [[Either]]: [[Left]]
      * if `r` is an [[Err]], and [[Right]] if `r` is [[Ok]] but `other` is not.
      * @group combine
      */
    def andTrace[U, E1](
        other: => Result[U, E1]
    ): Result[(T, U), Either[E, E1]] =
      r match
        case Err(error) => Err(Left(error))
        case Ok(t) =>
          other match
            case Ok(u)      => Ok((t, u))
            case Err(error) => Err(Right(error))

    /** Returns a tuple of `r` and `other` if both are [[Ok]], other return all
      * errors as a [[List]]. Does '''not''' short-circuit.
      * @group combine
      */
    def zip[U](other: Result[U, E]): Result[(T, U), List[E]] = (r, other) match
      case (Ok(t), Ok(u))     => Ok((t, u))
      case (Ok(_), Err(e))    => Err(List(e))
      case (Err(e), Ok(_))    => Err(List(e))
      case (Err(e1), Err(e2)) => Err(List(e1, e2))

    /** Generalized version of [[zip]] to work with arbitrary tuples.
      * @see
      *   [[zip]]
      * @group combine
      */
    infix def cons[Ts <: Tuple](
        other: Result[Ts, List[E]]
    ): Result[T *: Ts, List[E]] = (r, other) match
      case (Ok(t), Ok(ts))        => Ok(t *: ts)
      case (Err(e), Ok(_))        => Err(List(e))
      case (Ok(_), err @ Err(es)) => err
      case (Err(e), Err(es))      => Err(e :: es)

    /** Returns `r` if it is [[Ok]], otherwise evaluates and returns `other`.
      * @group combine
      */
    def or[E1](other: => Result[T, E1]) = r match
      case ok @ Ok(_) => ok
      case Err(_)     => other

    // transformers

    /** Maps the [[Ok]] value through `f`, otherwise keeping the [[Err]] error.
      * @group transform
      */
    def map[U](f: T => U): Result[U, E] = r match
      case Ok(value)    => Ok(f(value))
      case err @ Err(_) => err

    /** Maps the [[Err]] error through `f`, otherwise keeping the [[Ok]] value.
      * @group transform
      */
    def mapErr[E1](f: E => E1): Result[T, E1] = r match
      case ok @ Ok(_) => ok
      case Err(error) => Err(f(error))

    /** Returns the output of `f` from applying it to the [[Ok]] value,
      * otherwise keeping the [[Err]] case.
      * @group transform
      */
    def flatMap[U](f: T => Result[U, E]) = r match
      case Ok(value)    => f(value)
      case err @ Err(_) => err
  end extension

  extension [T, E](r: Result[Result[T, E], E])
    /** Flattens a nested [[Result]] with the same error type.
      * @group transform
      */
    def flatten: Result[T, E] = r.match
      case Ok(value)        => value
      case err @ Err(error) => err

  // Conversion to `Try`

  extension [T, E <: Throwable](r: Result[T, E])
    /** Converts the result to [[scala.util.Try]].
      * @group convert
      */
    def toTry: scala.util.Try[T] = r match
      case Ok(value)  => scala.util.Success(value)
      case Err(error) => scala.util.Failure(error)

  /** Evaluates `body`, catching [[scala.util.control.NonFatal NonFatal]]
    * exceptions as errors.
    * @see
    *   [[scala.util.Try Try.apply]] for a similar method returning
    *   [[scala.util.Try]].
    * @group construct
    */
  def catchException[T](body: => T): Result[T, Throwable] =
    try Ok(body)
    catch case NonFatal(ex) => Err(ex)

  /** Right unit for chains of [[cons]]. An [[Ok]] with an [[EmptyTuple]] value.
    * @group construct
    */
  val empty: Result[EmptyTuple, Nothing] = Ok(EmptyTuple)

  // Boundary and break

  /** Evaluates `body`, returning the output as an [[Ok]] value.
    *
    * Within `body`:
    *   - [[?]] can be used to unwrap [[Result]] values, with the body
    *     short-circuiting back with the error.
    *   - [[raise]] can be used to quickly short-circuit with an error.
    * @group eval
    */
  inline def apply[T, E](
      inline body: boundary.Label[Result[T, E]] ?=> T
  ): Result[T, E] =
    boundary(Ok(body))

  type From[T] = [U] =>> Conversion[T, U]

  /** Short-circuits the current `body` under [[Result$.apply Result.apply]]
    * with the given error.
    * @group eval
    */
  inline def raise[E, E1: From[E]](err: E)(using
      boundary.Label[Err[E1]]
  ): Nothing =
    boundary.break(new Err(err.convert))

  /** A shorthand to call [[scala.util.boundary.break boundary.break]] with a
    * [[Result]] label.
    */
  inline def break[T, E](inline r: Result[T, E])(using
      boundary.Label[Result[T, E]]
  ) = boundary.break(r)

  extension [T, E](r: Result[T, E])
    /** Unwraps the result, returning the value under [[Ok]]. Short-circuits the
      * current `body` under [[Result$.apply Result.apply]] with the given error
      * if the result is an [[Err]].
      * @group eval
      * @see
      *   [[apply]] and [[raise]].
      */
    transparent inline def ?[E1: From[E]](using boundary.Label[Err[E1]]): T =
      r match
        case Ok(value)  => value
        case Err(error) => raise(error)
end Result
