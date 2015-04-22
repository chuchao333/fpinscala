package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A] {
  // exercise 7
  def map[B](f: A => B): Either[E, B] =
    this match {
      // this Won't type check
      // case left: Left => left
      // this can type-check
      // case left: Left[E] => left
      // We need a new copy rather than the original reference here.
      case Left(get) => Left(get)
      case Right(get) => Right(f(get))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(get) => f(get)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      // case _: Left[E] => b
      case Left(_) => b
      // Do we have to return a new copy here? No.
      // case r: Right => r
      case Right(get) => Right(get)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }

  def map2ViaForComprehension[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // exercise 8
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil): Either[E, List[B]]) { (a, b) => f(a).map2(b)(_ :: _)}

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
}