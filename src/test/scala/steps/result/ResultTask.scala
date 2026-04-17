package steps.result

import steps.result.Result.eval.{check, ok, raise}

// test demonstration that Result.task is a useful optimisation for side-effecting operations
// such as consuming a mutable stream (i.e. on each pull we can do something useful, or error)

class ResultTaskTest extends munit.FunSuite {

  test("check that str is all same (Result.apply, ok)"):
    val c = Checker("aaa")
    assertEquals(c.isAllX('a'), Result.done)

  test("check that str is all same (Result.task, check)"):
    val c = Checker("aaa")
    assertEquals(c.isAllY('a'), Result.done)

  test("check fail that str is all same (Result.apply, ok)"):
    val c = Checker("abc")
    assertEquals(c.isAllX('a'), Result.Err(CheckErr("expected 'a'", 1)))

  test("check fail that str is all same (Result.task, check)"):
    val c = Checker("abc")
    assertEquals(c.isAllY('a'), Result.Err(CheckErr("expected 'a'", 1)))

}

case class CheckErr(msg: String, idx: Int)

/** Class to parse a string with a simple validation
  *
  * CFR decompilation output, observe that `isAllY` is much more compact and
  * does not allocate `new Result.Ok`:
  * ```java
  * public class Checker
  * extends CharStream {
  *     public Checker(String s) {
  *         super(s);
  *     }
  *
  *     public Result<BoxedUnit, CheckErr> isAllX(char x) {
  *         while (!this.isEOF()) {
  *             Result<BoxedUnit, CheckErr> result = this.expectChar(x);
  *             if (result instanceof Result.Ok) {
  *                 Result.Ok ok = (Result.Ok)result;
  *                 Result.Ok ok2 = Result.Ok$.MODULE$.unapply(ok);
  *                 BoxedUnit boxedUnit = (BoxedUnit)ok2._1();
  *                 BoxedUnit boxedUnit2 = BoxedUnit.UNIT;
  *                 BoxedUnit boxedUnit3 = boxedUnit;
  *                 if (!(boxedUnit2 != null ? !boxedUnit2.equals(boxedUnit3) : boxedUnit3 != null)) {
  *                     BoxedUnit value = boxedUnit;
  *                     continue;
  *                 }
  *             }
  *             if (result instanceof Result.Err) {
  *                 Result.Err err;
  *                 Result.Err err2 = err = (Result.Err)result;
  *                 return err2;
  *             }
  *             throw new MatchError(result);
  *         }
  *         return Result.Ok$.MODULE$.apply((Object)BoxedUnit.UNIT);
  *     }
  *
  *     public Result<BoxedUnit, CheckErr> isAllY(char y) {
  *         while (!this.isEOF()) {
  *             Result.Err err;
  *             Result<BoxedUnit, CheckErr> result = this.expectChar(y);
  *             if (!(result instanceof Result.Err)) continue;
  *             Result.Err err2 = err = (Result.Err)result;
  *             return err2;
  *         }
  *         return Result$.MODULE$.done();
  *     }
  *
  *     public Result<BoxedUnit, CheckErr> expectChar(char c) {
  *         if (this.currentChar() != c) {
  *             return new Result.Err((Object)CheckErr$.MODULE$.apply("expected '" + c + "'", this.currentOffset()));
  *         }
  *         this.advance();
  *         return Result$.MODULE$.done();
  *     }
  * }
  * ```
  */
class Checker(s: String) extends CharStream(s):
  /** using basic `apply` and `ok` */
  def isAllX(x: Char): Result[Unit, CheckErr] =
    Result:
      while !isEOF do expectChar(x).ok

  /** optimised with `Result.task` and `check` */
  def isAllY(y: Char): Result[Unit, CheckErr] =
    Result.task:
      while !isEOF do expectChar(y).check

  def expectChar(c: Char): Result[Unit, CheckErr] = Result.task:
    if currentChar == c then advance()
    else raise(CheckErr(s"expected '$c'", currentOffset))

class CharStream(s: String):
  private var currIdx: Int = -1
  private var currChar: Char = -1.toChar
  advance() // initialize
  def currentChar: Char = currChar
  def currentOffset: Int = currIdx
  def isEOF: Boolean = currIdx >= s.length()
  def advance(): Unit =
    val i = currIdx + 1
    if i < s.length() then currChar = s(i)
    else currChar = '\u0000'
    currIdx = i
