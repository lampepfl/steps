package steps.annotation

import scala.annotation.experimental

@experimental
final class FibonacciMemoize extends munit.FunSuite:

  @memoize
  def fib(n: Int): Int =
    println(s"compute fib of $n")
    if (n <= 1) return n
    var prePrevious = 1
    var previous = 1
    var i = 2

    while i < n do
      val temp = prePrevious + previous
      prePrevious = previous
      previous = temp
      i += 1

    previous
  end fib

  test("fib compute one the result one time for the same entry"):
      val output = new java.io.ByteArrayOutputStream
      Console.withOut(output):
          val r1 = fib(10)
          val r2 = fib(10)
          assert(r1 == r2) // check that the cache is indeed populated with the correct value
      assertNoDiff(output.toString(), "compute fib of 10") // check that we indeed run the function once

end FibonacciMemoize