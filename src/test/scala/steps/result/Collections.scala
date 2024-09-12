import steps.result.Result
import steps.result.Result.*
import steps.result.Result.eval.*

/** Tests working with collection. */
class CollectionTest extends munit.FunSuite {
  given [T]: Conversion[T, T] = identity

  test("tryMap on iterable") {
    extension [T](s: Iterable[T])
      def tryMap[U, E](f: T => Result[U, E]) =
        Result:
          s.map(f(_).ok)

    def getEven(v: Int) =
      if v % 2 == 0 then Ok(v) else Err("not even number")

    val mustEvens =
      (1 to 5)
        .tryMap(getEven)
        .map(_.toSeq)

    assertEquals(mustEvens, Err("not even number"))

    val evens =
      (1 to 5)
        .map(getEven)
        .flatMap(_.toOption)

    assertEquals(evens, Seq(2, 4).toIndexedSeq)
  }

  test("tryMap on tree") {
    enum Tree[T]:
      case Leaf(item: T)
      case Node(item: T, children: Tree[T]*)

    extension [T](t: Tree[T])
      def map[U](f: T => U): Tree[U] =
        t match
          case Tree.Leaf(item) => Tree.Leaf(f(item))
          case Tree.Node(item, children*) =>
            Tree.Node(f(item), children.map(_.map(f))*)

      def tryMap[U, E](f: T => Result[U, E]) =
        Result:
          t.map(f(_).ok)
  }
}
