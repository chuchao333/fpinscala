package fpinscala.laziness

import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument
  // by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // exercise 1
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def toListWithLocalBuf: List[A] = {
    val buf = ListBuffer.empty[A]

    def go(s: Stream[A]): Unit = s match {
      case Cons(h, t) => buf += h(); go(t())
      case _ => ()
    }

    go(this)
    buf.toList
  }

  // why don't have this helper function?
  def isEmpty: Boolean =
    this match {
      case Empty => true
      case _ => false
    }

  // exercise 2
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else {
      this match {
        case Empty => Empty
        case Cons(h, t) => Stream.cons[A](h(), t().take(n - 1))
      }
    }

  // the provided answer is more concise
  def take1(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take1(n - 1))
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else {
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n - 1)
      }
    }

  // the provided answer is more concise
  def drop1(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop1(n - 1)
      case _ => Empty
    }

  // exercise 3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        val head = h()
        if (p(head)) Stream.cons[A](head, t() takeWhile p)
        else Empty
      }
    }

  // exercise 4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b}

  // exercise 5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) => if (p(a)) Stream.cons(a, b) else b }

  // exercise 6
  def headOption: Option[A] =
    // this is dependent on the semantics of the 'foldRight' on Stream
    foldRight(None: Option[A]) { (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  // exercise 7
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B]) { (a, b) => Stream.cons(f(a), b) }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) => if (f(a)) Stream.cons(a, b) else b}

  // 1. why 'other' has to be non-strict?
  // 2. A function is contravariant on its arguments and covariant on its return type
  // def append(other: => Stream[A]): Stream[A] =
  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other) { (a, b) => Stream.cons(a, b) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B]) { (a, b) => f(a).append(b) }

  // exercise 13
  def mapViaUnFold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def takeViaUnFold(n: Int): Stream[A] =
    Stream.unfold((n, this)) {
      case (nn, s) =>
        if (nn <= 0) None
        else {
          s match {
            case Empty => None
            case Cons(h, t) => Some(h(), (n - 1, t()))
          }
        }
    }

  def takeWhileViaUnFold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      s => s match {
        case Empty => None
        case Cons(h, t) => {
          val head = h()
          if (f(head)) Some(head, t()) else None
        }
      }
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)(_ -> _)

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_, _))

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold(this -> s) {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())) -> (Empty, t()))
      case (Cons(h, t), Empty) => Some(f(Some(h()), None) -> (t(), Empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  // exercise 14
  def startsWith[B](s: Stream[B]): Boolean = (zip(s) takeWhile { case (a, b) => a == b }).toList.size == s.toList.size

  // exercise 15
  // The last element of 'tails' is always the empty Stream
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s @ Cons(h, t) => Some(s -> t())
    } append Stream(Stream.empty[A])

  // exercise 16
  // this is not very efficient, O(n^2)
  def scanRightInefficient[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails map { s => s.foldRight(z)(f) }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))) { (a, acc) =>
      // use lazy val here to ensure only one evaluation of the by-name arg in f.
      lazy val current = acc
      val zz = f(a, current._1)
      val next = Stream.cons(zz, current._2)
      (zz, next)
    } _2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // exercise 8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Note: this is more efficient than above
  def constantNoRecursiveCall[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // exercise 9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // exercise 10
  def fibs(): Stream[Int] = {
    def go(f1: Int, f2: Int): Stream[Int] =
      cons(f1, go(f1, f1 + f2))

    cons(0, go(1, 1))
  }

  // exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // exercises 12
  // 'z' could be any value during unfold, it won't be used at all
  val onesViaUnFold: Stream[Int] = unfold(0)(_ => Some(1, 0))

  def constantViaUnFold[A](a: A): Stream[A] = unfold(0)(_ => Some(a, 0))

  def fromViaUnFold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  // Why we have to write 'case (a, b)' here rather than just '(a, b)'
  // def fibsViaUnFold(): Stream[Int] = unfold((0, 1)) { (a, b) => Some(a, (b, a + b))}
  def fibsViaUnFold(): Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }
}