package fpinscala.state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng1) = rng.nextInt
    val res = if (v == Int.MinValue) Int.MaxValue else math.abs(v)
    (res, rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nonNeg, rng1) = nonNegativeInt(rng)
    (nonNeg.toDouble / (Int.MaxValue + 1), rng1)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(x => x.toDouble / (Int.MaxValue + 1))(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2.toDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1.toDouble, i2), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    val (i3, rng3) = rng2.nextInt

    ((i1.toDouble, i2.toDouble, i3.toDouble), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val buf = ListBuffer.empty[Int]

    @tailrec
    def go(cnt: Int, rng: RNG): (List[Int], RNG) = {
      if (cnt == 0) (buf.toList, rng)
      else {
        val (v, nextRng) = rng.nextInt
        buf += v
        go(cnt - 1, nextRng)
      }
    }

    go(count, rng)
  }

  // exercise 6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  // exercise 7
  def sequenceViaRecursion[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val buf = ListBuffer.empty[A]

    def go(rng: RNG, fss: List[Rand[A]]): RNG = {
      fss match {
        case h :: t => {
          val (v, nextRng) = h(rng)
          buf += v
          go(nextRng, t)
        }
        case _ => rng
      }
    }

    val res: Rand[List[A]] = rng => {
      val finalRng = go(rng, fs)
      (buf.toList, finalRng)
    }
    res
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A])) { (cur, acc) => map2(cur, acc)(_ :: _) }

  // exercise 8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // exercise 9
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def map2ViaFlatMap[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(sa) { a => map(sb) { b => f(a, b) } }
}

case class State[S,+A](run: S => (A, S)) {
  // exercise 10
  def map[B](f: A => B): State[S, B] =
    flatMap { a => State.unit(f(a)) }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a => sb map { b => f(a, b) } }

  def map_1[B](f: A => B): State[S, B] = State { s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def map2_1[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a) run s1
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State { s =>
    @tailrec
    def go[S, A](s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) = {
      actions match {
        case h :: t => {
          val (v, nextState) = h.run(s)
          go(nextState, t, v :: acc)
        }
        case _ => (acc.reverse, s)
      }
    }

    go(s, ss, List[A]())
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
