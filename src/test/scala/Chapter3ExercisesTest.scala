import Chapter3Exercises._
import org.scalatest.funsuite.AnyFunSuite

class Chapter3ExercisesTest extends AnyFunSuite {

  test("Exercise 3.1 - What is the value of x? Answer: 3") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    assert(x == 3)
  }

  test("Exercise 3.2 - Implement tail function in O(1) time.") {
    val newTail = tail(List(1, 2, 3, 4, 5))

    assert(newTail == List(2, 3, 4, 5))
    assert(newTail == Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
  }

  test("Exercise 3.3 - Implement setHead using pattern matching.") {
    val testList = List(1, 2, 3, 4, 5)

    assert( setHead(testList, 0) == List(0, 2, 3, 4, 5))
  }

  test("Exercise 3.4 - Implement drop function using tail recursion and pattern matching.") {
    val testList = List(1, 2, 3, 4, 5)

    assert( drop(testList, n = 0) == testList )
    assert( drop(testList, n = 1) == List(2, 3, 4, 5) )
    assert( drop(testList, n = 3) == List(4, 5) )
    assert( drop(testList, n = 7) == Nil )
  }

  test("Exercise 3.5 - Implement dropWhile using tail recursion and pattern matching.") {
    val testList = List(1, 2, 3, 4, 5)

    assert( dropWhile(testList, (a: Int) => a < 0) == List(1, 2, 3, 4, 5) )
    assert( dropWhile(testList, (a: Int) => a < 3) == List(3, 4, 5) )
    assert( dropWhile(testList, (a: Int) => a < 7) == Nil )
  }

  test("Exercise 3.6 - Remove last element of list in O(n) time using pattern matching.") {

    assert( init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4) )
    assert( init(List(1)) == Nil )
    assert( init(List()) == Nil )
  }

  test("Exercise 3.7 - Implement product using foldRight.") {
    // we cannot have early termination when hitting a 0.0 in our list unless we alter the implementation.
    assert( product(List(1, 2, 3, 4)) == 24 )
    assert( product(List(1, 2, 3, 0, 4, 5)) == 0 )
  }

  test("Exercise 3.9 - Implement length using foldRight.") {
    assert( length(List(1)) == 1 )
    assert( length(List(1, 2, 3, 4)) == 4 )
    assert( length(List(1, 2, 3, 4, 5, 6, 7, 8)) == 8 )
    assert( length(List()) == 0 )
    assert( length(Nil) == 0 )
  }
}
