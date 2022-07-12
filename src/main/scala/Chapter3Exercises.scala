
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

object Chapter3Exercises {

  /**
   * Compute sum of elements in the list.
   */
  def sum(as: List[Int]): Int = {
    as match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  /**
   * Return the tail of the list.
   */
  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  /**
   * Replace the head element of the list.
   */
  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, t) => Cons(head, t)
    }
  }

  /**
   * Drop the first n elements of a list.
   */
  def drop[A](as: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], n: Int): List[A] = {
      if (n == 0) as
      else {
        as match {
          case Nil => Nil
          case Cons(_, t) => loop(t, n-1)
        }
      }
    }
    loop(as, n)
  }

  /**
   * Drop elements in the list from left to right while the condition is true.
   */
  def dropWhile[A](as: List[A], condition: (A) => Boolean): List[A] = {
    @annotation.tailrec
    def loop(as: List[A], condition: (A) => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(h, t) =>
          if (condition(h)) loop(t, condition)
          else Cons(h, t)
      }
    }
    loop(as, condition)
  }

  /**
   * Remove the last element in a list using pattern matching.
   */
  def init[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /**
   * Implement product function using foldRight.
   */
  def product(as: List[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  /**
   * Implement length function using foldRight.
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, x2) => 1 + x2)
  }
}
