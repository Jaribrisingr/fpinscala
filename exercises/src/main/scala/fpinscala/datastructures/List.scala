package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => t
      case Nil => List()
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
   if(n <= 0) l
   else l match {
        case Nil => List()
        case Cons(h, t) => drop(t, n-1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => List()
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => List()
      case Cons(h, t) if length(t) > 1 => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => 1+acc)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  //exercise 3.11
  def leftSum(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }
  def leftProduct(ns: List[Double]) = {
    foldLeft(ns, 1.0)(_ * _)
  }
  def leftLength[A](l: List[A]) = {
    foldLeft(l, 0)((acc, h) => 1+ acc)
  }

  //exercise 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List())((acc, h) => Cons(h, acc))
  }

  //exercise 3.13
  //exercise 3.14
  def leftAppend[A](l: List[A], r: List[A]): List[A] = {
    foldLeft(l, r)((acc, h) => Cons(h, r))
  }
  def rightAppend[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  // exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
  }

  // exercise 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  }

  //exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
  }

  //exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    //foldRight(as, Nil:List[B])((h,t) => f(h))
    concat(map(as)(f))
  }

  //exercise 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // exercise 3.22
  def addTogether(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addTogether(t1, t2))
    }
  }

  //exercise 3.23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }
}
