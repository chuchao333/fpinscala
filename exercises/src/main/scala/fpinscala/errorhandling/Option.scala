package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _}

// exercise 1
sealed trait Option[+A] {
  // exercise 1
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(get) => Some(f(get))
    }

  // a method is contravariant on its result type
  def getOrElse[B>:A](default: => B): B =
    this match {
      case None => default
      case Some(get) => get
    }

  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this map Some.apply getOrElse(ob)
  // the scala.Option implements the 'orElse' return the reference ('this') rather than a new copy of non-None
  def orElse1[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def filter(f: A => Boolean): Option[A] = this flatMap { x => if (f(x)) this else None }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // exercise 2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs.map(x => Math.pow(x - m, 2))) }

  // exercise 3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(aGet), Some(bGet)) => Some(f(aGet, bGet))
      case _ => None
    }

  def map2ViaFlatMapAndMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap { aa => b map { bb => f(aa, bb) } }

  def map2ViaForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // exercise 4
  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map { p => (s: String) => p.matcher(s).matches() }

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2)) { (m1, m2) => m1(s) && m2(s) }

  // exercise 5
  // sequenceViaTraverse (this is not so obvious as it looks like)
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  // exercise 6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]]) { (a, b) => map2(f(a), b) { (x: B, xs: List[B]) => x :: xs } }
}