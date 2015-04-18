package fpinscala.datastructures

import scala.annotation.tailrec

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
    // The below will be matched with x and y bound to 1 and 2 respectively, thus the result is 3
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // @tailrec this is not a tail recursive function, adding @tailrec will fail to compile
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // exercise 2
  // The alternative could be to return None when the input list is empty.
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("Tail on an empty list!")
      case Cons(_, t) => t
    }

  // exercise 3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("Couldn't set head on an empty list")
      case Cons(_, t) => Cons(h, t)
    }

  // exercise 4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Negative number of elements to drop.")

    if (n == 0) l
    else {
      l match {
        case Nil => sys.error("No more elements to drop")
        case Cons(_, t) => drop(t, n-1)
      }
    }
  }

  // exercise 5
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
//    l match {
//      case Nil => Nil
//      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
//    }
    // more concise
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t)(f)
      case _ => l
    }
  }

  // exercise 6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error ("init of an empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (_, s) => s + 1 }

  // exercise 10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // exercise 11
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  // exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (acc, current) => Cons(current, acc)}

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  // exercise 14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) { (cur, acc) => Cons(cur, acc)}

  // exercise 15
  def concat[A](lists: List[List[A]]): List[A] =
    // lists match {
    //   case Nil => Nil
    //   case (h, t) => foldLeft(t, h)(append)
    // }
    // Note: we should use foldRight since 'append' is linear with its first argument
    foldRight(lists, Nil: List[A])(append)

  // exercise 18
  // @tailrec this one is not tail recursive
  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  def map1[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]) { (x, acc) => Cons(f(x), acc) }

  // exercise 19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }

  def filter1[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A]) { (x, acc) => if (f(x)) Cons(x, acc) else acc }

  // exercise 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    l match {
      case Nil => Nil
      // Other efficient implementation?
      case Cons(h, t) => append(f(h), flatMap(t)(f))
    }

  def flatMap1[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // exercise 21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l) { x => if (f(x)) Cons(x, Nil) else Nil}

  // exercise 23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  // exercise 24
  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec
    def matchToEnd[A](l1: List[A], l2: List[A]): Boolean =
      (l1, l2) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && matchToEnd(t1, t2)
        case _ => false
      }

    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h, t), Cons(hSub, tSub)) => if (h == hSub) matchToEnd(t, tSub) else hasSubsequence(t, sub)
    }
  }
}
