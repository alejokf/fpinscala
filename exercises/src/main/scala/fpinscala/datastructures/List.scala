package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working Via lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting Via `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  // This should be 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendF[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, l1) => Cons(x, l1))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Nil doesn't have a tail")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Nil doesn't have a head")
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Nil does not have elements")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, xs) => 1 + length(xs)
  }

  def lengthFR[A](l: List[A]): Int =
    foldRight(l, 0)((a, b) => b + 1)

  def sumFR(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def productFR(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFL(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productFL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((a, b) => a + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  // We need to reverse the list, and also reverse the order in which the params of f are applied
  def foldLeftFRAndReverse[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((b, a) => f(a, b))

  // We need to reverse the list, and also reverse the order in which the params of f are applied
  def foldRightFLAndReverse[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  /*
  The accumulator z is a function B => B. f will be then a function that:
  - takes a parameter (a, accF): a of type A and accF Via the same type than the accumulator function described above.
  - returns a function that receives B and returns another function by applying the function f on (b, a) - note the order -
   and the apply accF to the result of that.
  At the end this will result in a function that we need to apply to a number: the empty initial accumulator z.
   */
  def foldLeftFR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, accF) => b => accF(f(b, a)))(z)

  /*
  The accumulator z is a function B => B. f will be then a function that:
  - takes a parameter (accF, a): accF Via the same type than the accumulator function described above and a of type A.
  - returns a function that receives B and returns another function by applying the function f on (a ,b) - note the order -
  and the apply accF to the result of that.
  At the end this will result in a function that we need to apply to a number: the empty initial accumulator z.
   */
  def foldRightFL[A, B](l: List[A], z: B)(f: (A ,B) => B): B =
    foldLeft(l, (b: B) => b)((accF, a) => b => accF(f(a, b)))(z)

  def concat[A](l: List[List[A]]): List[A] =
    foldRightFL(l, List[A]())((xs, ys) => append(xs, ys))

  def add1(l: List[Int]): List[Int] =
    foldRightFL(l, List[Int]())((x, xs) => Cons(x + 1, xs))

  def doubleToString(l: List[Double]): List[String] =
    foldRightFL(l, List[String]())((x, xs) => Cons(x.toString, xs))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightFL(l, List[B]())((x, xs) => Cons(f(x), xs))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightFL(l, List[A]())((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if(f(x)) List(x) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons( x + y, addPairwise(xs, ys))
  }

  def zipVia[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipVia(xs, ys)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(_, xs) => if (startsVia(sup, sub)) true else hasSubsequence(xs, sub)
  }

  @annotation.tailrec
  def startsVia[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => startsVia(xs, ys)
    case _ => false
  }

}
