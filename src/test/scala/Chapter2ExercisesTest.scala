import Chapter2Exercises._
import org.scalatest.funsuite.AnyFunSuite

class Chapter2ExercisesTest extends AnyFunSuite {

  test("Exercise 2.1 - Write a recursive function for getting the nth fibonacci number.") {
    Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
      .zipWithIndex
      .foreach{ case(fibNum, i) =>
        assert( fib(i) == fibNum )
      }
  }

  test("Exercise 2.2 - Implement isSorted using tail recursion.") {
    val a1: Array[Int] = Array(1, 2, 3, 4, 5)
    val a2: Array[Int] = Array(1, 2, 7, 4, 5)

    def compare(x1: Int, x2: Int): Boolean = x1 > x2

    assert( isSorted(a1, compare) )
    assert( !isSorted(a2, compare) )
  }

  test("Exercise 2.3 - Implement currying for 2 parameter functions.") {
    val x1: Int = 7
    val x2: Int = 5

    def add(x1: Int, x2: Int): Int = x1 + x2

    assert( curry(add)(x1)(x2) == add(x1, x2) )
  }

  test("Exercise 2.4 - Implement reverse currying for 2 parameter functions.") {
    val x1: Int = 3
    val x2: Int = 8

    def multiply(x1: Int, x2: Int): Int = x1 * x2

    assert( uncurry(curry(multiply))(x1, x2) == multiply(x1, x2) )
  }

  test("Exercise 2.5 - Implement higher-order function for composing two single-parameter functions.") {

    def feetToMeters(f: Double): Double = f / 3.281
    def circumference(r: Double): Double = 2 * math.Pi * r
    def circumferenceInMeters(feet: Int): Double = compose(circumference, feetToMeters)(feet)

    val testRadius: Int = 15
    val testCircumference: Int = 28

    assert( math.floor(circumferenceInMeters(testRadius)) == testCircumference )
  }
}
