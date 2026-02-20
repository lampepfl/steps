/** Defines [[Result]], an enum representing either a success value or an error.
  */
package steps.result

import language.experimental.captureChecking

import java.{util => ju}
import scala.util.control.NonFatal
import scala.util.boundary
import scala.annotation.implicitNotFound
import scala.Conversion.into
import scala.util.boundary.Label

/** Represents either a success value of type `T` or an error of type `E`. It
  * can be used when expecting results that have errors that should be inspected
  * by the caller. For this, [[Result]] have a precise error type parameter.
  *
  * To create one, directly use one of the variant constructors [[Result.Ok Ok]]
  * or [[Result.Err Err]], or start a computation scope with [[Result.apply]]:
  *  ```
  *  extension[T] (it: IterableOnce[T])
  *    def tryMap[U, E](f: T => Result[U, E]): Result[Seq[U], E] =
  *      Result:
  *        it.iterator.map(f(_).ok) // shorts-circuit on the first Err and returns
  *  ```
  *
  * Tail-recursive functions can be implemented by using
  * [[Result.eval.break eval.break]]:
  *
  *  ```
  *  extension[T] (seq: Seq[T])
  *    @scala.annotation.tailrec
  *    def tryFoldLeft[U, E](init: U)(f: (U, T) => Result[U, E]) =
  *      Result:
  *        seq match
  *          case Seq() => init
  *          case Seq(h, t*) => eval.break(t.tryFoldLeft(f(init, h).ok)(f))
  *
  *  // however, a much simpler implementation is
  *  extension[T] (it: IterableOnce[T])
  *    def tryFoldLeft[U, E](init: U)(f: (U, T) => Result[U, E]) =
  *      Result:
  *        it.iterator.foldLeft(init)(f(_, _).ok)
  *  ```
  *
  * Conversions from [[Option]] and [[Either]] are available in
  * [[ScalaConverters]] (or implicitly through importing [[Conversions]]).
  *
  *  ```
  *  // from Option
  *  val opt: Option[Int] = f()
  *  val res1 = opt.okOr(Error.NotFound) // with explicit error
  *  val res2 = opt.asResult // Result[Int, NoSuchElementException]
  *  // to Option
  *  val opt2 = res1.ok // returns Option[Int]
  *
  *  // from Either
  *  val either: Either[E, T] = f()
  *  val res = either.asResult // Result[T, E]
  *  // to Either
  *  val either2 = res.toEither
  *
  *  // from Try
  *  val t: Try[T] = Try { /* ... */ }
  *  val res = t.asResult // Result[T, Throwable]
  *  // to Try
  *  val t2 = res.toTry // Try[T], if error type is throwable
  *  val t2 = Try { res.get } // Try[T], throws ResultIsErrException
  *  ```
  *
  * Casual usage in a library where precise error reporting is preferred would
  * consist of creating the Error type as an `enum` or an union type, aliasing
  * the `Result` type with the correct error type, and using it as part of the
  * function signature.
  *
  *  ```
  *  enum LibError:
  *    case A
  *    case B(inner: SomeError)
  *  // or ...
  *  type LibError = ErrorA.type | SomeError
  *
  *  type LibResult[+T] = Result[T, LibError]
  *
  *  object LibResult:
  *    import scala.util.boundary
  *    export Result.{apply as _, *}
  *
  *    // override `apply` manually, to have it fix the Error type parameter.
  *    inline def apply[T](inline body: boundary.Label[LibResult[T]] => T) = Result.apply(body)
  *
  *  // in the library:
  *  def ApiEndpoint(p: Int): LibResult[String] =
  *    LibResult:
  *      // ...
  *  ```
  *
  * In end applications, prefer either:
  *   - directly `match`ing over the [[Result]], where precise errors need to be
  *     inspected.
  *   - in other cases, where tracing is wanted and error details are less
  *     important, prefer unwrapping the [[Result]] directly and catch the
  *     [[Result.ResultIsErrException]] at the top level to inspect the error
  *     details.
  *
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
enum Result[+T, +E] extends IterableOnce[T]:
  /** Contains the success value */
  case Ok[+T](value: T) extends Result[T, Nothing]

  /** Contains the error value */
  case Err[+E](error: E) extends Result[Nothing, E]

  // IterableOnce implementation

  def iterator: Iterator[T] = this match
    case Ok(value) => Iterator(value)
    case Err(_)    => Iterator.empty

  override def knownSize: Int = this match
    case Ok(_)  => 1
    case Err(_) => 0

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
  /** Convert the error channel of a [[Result]]. Used for example with [[Result.eval.break eval.break]] */
  given convertErrChannel: [T, E, E1] => Conversion[E, E1] => Conversion[Result[T, E], Result[T, E1]] =
    _.mapErr(_.convert)

  /** An exception obtained by calling [[Result.get get]] on a [[Result.Err]].
    * @param error
    *   the error value that was enclosed in the variant.
    */
  case class ResultIsErrException(error: Any)
      extends java.util.NoSuchElementException(
        s"Attempting to call `.get` on a Result.Err value of: $error"
      )

  /** Method implementations on [[Result]]. */
  extension [T, E](r: Result[T, E])

    // Basic case differentiation

    /** Returns whether the result is [[Result.Ok Ok]].
      * @group access
      */
    def isOk: Boolean = r.isInstanceOf[Ok[?]]

    /** Returns whether the result is [[Result.Err Err]].
      * @group access
      */
    def isErr: Boolean = r.isInstanceOf[Err[?]]

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

    /** Returns the [[Err]] error from the result.
      * @group convert
      */
    def errOption: Option[E] = r match
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

    /** Converts the result into an [[scala.Either Either]]. Where the [[Ok]] value is mapped to [[scala.Right]],
      * and the [[Err]] error to [[scala.Left]].
      * @group convert
      */
    def toEither: Either[E, T] = r match
      case Ok(value)  => Right(value)
      case Err(error) => Left(error)

    // Extracting values forcefully

    /** Returns the [[Ok]] value from the result, and throws an exception if it
      * is an error.
      * @throws ResultIsErrException
      *   if the result is an [[Err]].
      * @group access
      */
    def get: T = r match
      case Ok(value) => value
      case Err(error) =>
        throw ResultIsErrException(error)

    /** Returns the error from the result, and throws an exception if it is an
      * [[Ok]] value.
      * @throws java.util.NoSuchElementException
      *   if the result is an [[Ok]].
      * @group access
      */
    def getErr: E = r match
      case Err(error) => error
      case Ok(value) =>
        throw ju.NoSuchElementException(
          s"Expected Result.Err, got value: $value"
        )

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
    def and[U, E1](other: => Result[U, E1]): Result[(T, U), E | E1] = r match
      case err @ Err(_) => err
      case Ok(t) =>
        other match
          case Ok(u)      => Ok((t, u))
          case err @ Err(_) => err

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

    /** Generalized version of [[zip]] to work with arbitrary tuples.
      * Operator version of [[cons]]
      * @see
      *   [[zip]], [[cons]]
      * @group combine
      */
    infix def `*:`[Ts <: Tuple](
        other: Result[Ts, List[E]]
    ): Result[T *: Ts, List[E]] = r.cons(other)

    /** Returns `r` if it is [[Ok]], otherwise evaluates and returns `other`.
      * @group combine
      */
    def or[E1](other: => Result[T, E1]): Result[T, E1] = r match
      case ok: Ok[T]  => ok
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
      case ok: Ok[T]  => ok
      case Err(error) => Err(f(error))

    /** Returns the output of `f` from applying it to the [[Ok]] value,
      * otherwise keeping the [[Err]] case.
      * @group transform
      */
    def flatMap[U](f: T => Result[U, E]): Result[U, E] = r match
      case Ok(value)    => f(value)
      case err @ Err(_) => err

    /** Returns the output of `f` from applying it to the [[Err]] case error,
      * otherwise keeping the [[Ok]] case. Similar to [[flatMap]], but on the
      * [[Err]] case.
      * @group transform
      */
    def handleErr[E1](f: E => Result[T, E1]): Result[T, E1] = r match
      case ok: Ok[T]  => ok
      case Err(error) => f(error)

  end extension

  extension [T, E](r: Result[Result[T, E], E])
    /** Flattens a nested [[Result]] with the same error type.
      * @group transform
      */
    def flatten: Result[T, E] = r.match
      case Ok(value)   => value
      case err: Err[E] => err

  extension [T <: AnyRef, E](r: Result[T, E])
    /** Unwraps the inner value, returing `null` if there is an error.
      * @group access
      */
    def getNullable: T | Null = r match
      case Ok(value)  => value
      case err: Err[E] => null

  // Conversion to `Try`

  extension [T](r: Result[T, Throwable])
    /** Converts the result to [[scala.util.Try]].
      * @group convert
      */
    def toTry: scala.util.Try[T] = r match
      case Ok(value)  => scala.util.Success(value)
      case Err(error) => scala.util.Failure(error)

  /** Evaluates `body`, catching exceptions accepted by `catcher` as errors.
    * @see
    *   [[scala.util.Try Try.apply]] for a similar method returning
    *   [[scala.util.Try]], but works on all [[scala.util.control.NonFatal NonFatal]] exceptions.
    * @group construct
    */
  def catchException[T, E](catcher: PartialFunction[Throwable, E]^)(
      body: => T
  ): Result[T, E] =
    try Ok(body)
    catch case ex => Err(catcher.applyOrElse(ex, _ => throw ex))

  /** Right unit for chains of [[cons]]. An [[Ok]] with an [[EmptyTuple]] value.
    * @group construct
    */
  val empty: Result[EmptyTuple, Nothing] = Ok(EmptyTuple)

  /** Construct a `Result[T, E]` based on the condition given in `test`.
    * If the `test` succeeds (i.e. `test` is `true`), [[Ok]] with `ifTrue` is returned;
    * otherwise, return [[Err]] with `ifFalse`.
    * @group construct
    */
  inline def cond[T, E](
    test: Boolean,
    ifTrue: => T,
    ifFalse: => E,
  ): Result[T, E] = if test then Ok(ifTrue) else Err(ifFalse)

  // Boundary and break

  // Currently under its own object `eval` due to https://github.com/scala/scala3/issues/20126

  /** Evaluates `body`, returning the output as an [[Ok]] value.
    *
    * Within `body`:
    *   - [[eval.ok]] can be used to unwrap [[Result]] values, with the body
    *     short-circuiting back with the error.
    *   - [[eval.raise]] can be used to quickly short-circuit with an error.
    * @group eval
    */
  inline def apply[T, E](
      inline body: boundary.Label[Result[T, E]] ?=> T
  ): Result[T, E] = eval(body)

  /** Operations that are valid under a [[Result.apply]] scope.
    * @group eval
    * @see
    *   [[eval.ok]], [[eval.raise]] and [[eval.break]]
    */
  object eval:
    /** Similar to [[Result.apply]]. */
    inline def apply[T, E](
        inline body: boundary.Label[Result[T, E]] ?=> T
    ): Result[T, E] =
      boundary(Ok(body))

    /** Short-circuits the current `body` under [[Result$.apply Result.apply]]
      * with the given error.
      * @group eval
      */
    inline def raise[E](using
        @implicitNotFound(
          "`raise` cannot be used outside of the `Result.apply` scope."
        )
        label: boundary.Label[Err[E]]
    )(inline err: into[E]): Nothing =
      boundary.break(new Err(err))

    /** A shorthand to call [[scala.util.boundary.break boundary.break]] with a
      * [[Result]] label. This is useful in case an early return is needed, or a
      * tail recursion call (returning a [[Result]]) is called.
      *
      * For example, `tryFoldLeft` on [[Seq]] can be implemented like below:
      * {{{
      * extension[T] (seq: Seq[T])
      *   @scala.annotation.tailrec
      *   def tryFoldLeft[U, E](init: U)(f: (U, T) => Result[U, E]): Result[U, E] =
      *     Result:
      *       seq match
      *         case Seq() => init
      *         case Seq(h, t*) =>
      *           // t.tryFoldLeft(f(init, h).ok)(f).ok        // error: not a tail call (.ok applied at the end)
      *           eval.break(t.tryFoldLeft(f(init, h).ok)(f)) // ok
      * }}}
      *
      * Note that however, in most cases, it is simpler to capture the
      * [[Result.apply]] from outside of the
      * [[scala.annotation.tailrec tailrec]] loop, as this minimizes the amount
      * of boxing/unboxing [[Result]] needed on every step:
      * {{{
      * extension[T] (seq: Seq[T])
      *   def tryFoldLeft[U, E](init: U)(f: (U, T) => Result[U, E]): Result[U, E] =
      *     Result:
      *       @scala.annotation.tailrec
      *       def loop(current: U, seq: Seq[T]): U = // note the return type
      *         seq match
      *           case Seq() => current
      *           case Seq(h, t*) => loop(f(current, h).ok, t)
      *       loop(init, seq)
      * }}}
      *
      * Of course, one could also simply rewrite it in terms of `.foldLeft`:
      * {{{
      * extension[T] (it: IterableOnce[T])
      *   def tryFoldLeft[U, E](init: U)(f: (U, T) => Result[U, E]): Result[U, E] =
      *     Result:
      *       it.iterator.foldLeft(init)(f(_, _).ok)
      * }}}
      *
      * @return
      *   Always returns [[Nothing]], but the return type is set so that Scala
      *   does not infer `T` and `E` contravariantly.
      */
    inline def break[T, E](using
        @implicitNotFound(
          "`break` cannot be used outside of a corresponding `Result.apply` scope."
        )
        label: boundary.Label[Result[T, E]]
    )(inline r: into[Result[T, E]]): Nothing =
      boundary.break(r)
      // compiletime.summonFrom:
      //   case l: boundary.Label[Result[T, E]] => boundary.break(r)(using l)
      //   case l: boundary.Label[Result[Nothing, Nothing]] => ???
      //   case _ => ???

    private inline def breakErr[E](using
        @implicitNotFound(
          "`break` cannot be used outside of a corresponding `Result.apply` scope."
        )
        label: boundary.Label[Err[E]]
    )(inline r: Err[E]): Nothing =
      boundary.break(r)

//     private object breakImpl:
//       import scala.quoted.*
//       def apply[T, E](r: Expr[Result[T, E]])(using Quotes, Type[T], Type[E]) =
//         Expr.summon[boundary.Label[Result[T, E]]^{}] match
//           case Some(l) => '{ boundary.break(${ r })(using ${ l }) }
//           case None =>
//             Expr.summon[boundary.Label[Result[Nothing, Nothing]]^{}] match
//               case Some(label) =>
//                 label match
//                   case '{ $label: boundary.Label[Result[lt, le]]^{} } =>
//                     type gotLabel = Result[lt, le]
//                     val resultsIncompatible =
//                       if Expr.summon[T <:< lt].isEmpty then
//                         Seq(s"  - The result types are not compatible: `${Type
//                             .show[T]}` is not a sub-type of `${Type.show[lt]}`")
//                       else Seq()
//                     val errorsIncompatible =
//                       if Expr.summon[E <:< le].isEmpty then
//                         Seq(s"  - The error types are not compatible: `${Type
//                             .show[E]}` is not a sub-type of `${Type.show[le]}`")
//                       else Seq()
//                     quotes.reflect.report.errorAndAbort(
//                       s"""`break` cannot be used here with a value of type `${Type
//                           .show[Result[T, E]]}`,
// as the return type of the current `Result.apply` scope is different: `${Type
//                           .show[gotLabel]}`:
// ${(resultsIncompatible ++ errorsIncompatible).mkString("\n")}

// Perhaps you want to:
//   - Unwrap the `Result`, returning `${Type.show[T]}`:

//     ${r.show}.ok

//   - Map the resulting value to another type with `.map` or `.mapError`

//     ${r.show}
//       .map(value => ???)
//     ${r.show}
//       .mapError(error => ???)

// """
//                     )
//               case None =>
//                 quotes.reflect.report.errorAndAbort(
//                   "`break` cannot be used outside of a `Result.apply` scope."
//                 )

    extension [T, E](using
        @implicitNotFound(
          "`.ok` cannot be used outside of the `Result.apply` scope."
        )
        label: boundary.Label[Err[E]]
      )(inline r: into[Result[T, E]])
      /** Unwraps the result, returning the value under [[Ok]]. Short-circuits
        * the current `body` under [[Result$.apply Result.apply]] with the given
        * error if the result is an [[Err]].
        *  ```
        *  val ok: Result[Int, Nothing] = Ok(1)
        *  val err: Result[Int, String] = Err("fail!")
        *
        *  val compute = Result:
        *    ok.ok      // ok, unwraps and gives 1
        *      + err.ok // error, immediately sets compute to Err("fail")
        *      + 23     // not evaluated
        *  ```
        * @group eval
        * @see
        *   [[apply]] and [[raise]].
        */
      inline def ok: T = r match
        case Ok(value)   => value
        case err: Err[E] => breakErr(err)

    // Separate design of ok that does this conversion hackery just to let you pass label explicitly,
    // it seems better to work with `jumpTo` instead that can co-erce the type inference without this extra conversion type.

    // extension [T, E, E1](inline r: Result[T, E])
    //   /** Unwraps the result, returning the value under [[Ok]]. Short-circuits
    //     * the current `body` under [[Result$.apply Result.apply]] with the given
    //     * error if the result is an [[Err]].
    //     * {{{
    //     * val ok = Ok(1)
    //     * val err = Err("fail!")
    //     *
    //     * val compute = Result:
    //     *   ok.ok      // ok, unwraps and gives 1
    //     *     + err.ok // error, immediately sets compute to Err("fail")
    //     *     + f()   // not evaluated
    //     * }}}
    //     * @group eval
    //     * @see
    //     *   [[apply]] and [[raise]].
    //     */
    //   inline def ok(using
    //     @implicitNotFound(
    //       "`.ok` cannot be used outside of the `Result.apply` scope."
    //     )
    //     label: boundary.Label[Err[E1]]
    //   )(using inline conv: ConvertErr[E, E1]): T = r match
    //     case Ok(value)   => value
    //     case err: Err[E] => breakErr(ConvertErr.convert(err))

    // /** Conversion from error type `E` to error type `E1`.
    //   * An instance is provided for every `E1 <: E`, as well as for every existing given
    //   * [[scala.Conversion Conversion]] instance from `E` to `E1`.
    //   */
    // opaque type ConvertErr[E, E1] = Conversion[Err[E], Err[E1]]

    // /** Companion for [[Result.eval.ConvertErr ConvertErr]] */
    // object ConvertErr:

    //   /** Error conversion that performs no operation, for `E1 <: E`. */
    //   given IdentityConvertErr: [E <: E1, E1] => ConvertErr[E, E1] =
    //     identity

    //   /** Error conversion that delegates to an existing [[scala.Conversion Conversion]]
    //     * instance from `E` to `E1`.
    //     */
    //   given DelegateConvertErr: [E, E1]
    //     => (delegate: Conversion[E, E1])
    //     => ConvertErr[E, E1] =
    //       case Err(err) => Err(err.convert)

    //   /** Apply the `conv` to `err`, zero cost for `E <: E1`. */
    //   inline def convert[E, E1](inline err: Err[E])(using inline conv: ConvertErr[E, E1]): Err[E1] =
    //     inline compiletime.erasedValue[Err[E]] match
    //       case _: Err[E1] =>
    //         // trick to avoid actually materializing at runtime the identity conversion.
    //         // FIXME (scala/scala3): for some reason cast is needed here for inline reducer
    //         // to actually pick this branch
    //         err.asInstanceOf[Err[E1]]
    //       case _ => conv.convert(err)
end Result
