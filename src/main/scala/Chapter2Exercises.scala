object Chapter2Exercises {

  /**
   * A function for computing the nth Fibonacci number using tail recursion.
   * @param n indicates which number to return.
   * @return the nth Fibonacci number.
   */
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int , curr: Int): Int = {
      n match {
        case 0 => prev
        case _ => loop(n - 1, curr, prev + curr)
      }
    }
    loop(n, 0, 1)
  }

  /**
   * This function uses tail recursion to determine if an
   * array is properly sorted according to function f.
   * @param as the array to be evaluated.
   * @param f should indicate False if the elements are not sorted correctly.
   * @return true if sorted; otherwise false
   */
  def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i == as.length - 1) true
      else if (f(as(i), as(i+1))) false
      else go(i + 1)
    }
    go(0)
  }

  /**
   * Curry a function of 2 parameters.
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * Uncurry a function of 2 parameters.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Compose 2 single-parameter functions.
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
