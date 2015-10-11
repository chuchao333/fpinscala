package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop {
//  def check: Boolean
//
//  // exercise 8.3
//  def &&(p: Prop): Prop = new Prop {
//    // Prop.this refers the 'outer' this
//    override def check: Boolean = Prop.this.check && p.check
//  }
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case f: Falsified => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failedMsg, count) => p.tag(failedMsg).run(max, n, rng)
      case p @ (Proved | Passed) => p
    }
  }

  // Tag an extra error message (by prepending) to the original error message in case of failure
  private def tag(errorMsg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failedMsg, count) => Falsified(errorMsg + "\n" + failedMsg, count)
      case Passed => Passed
    }
  }
}

object Prop {
  // Type aliases like these can help the readability of the APIs
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  // type Result = Option[(FailedCase, SuccessCount)]

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max: MaxSize, n: TestCases, rng: RNG) => {
      randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(x => x.isFalsified).getOrElse(Passed)
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max: MaxSize, n: TestCases, rng: RNG) => {
      // for each size, generate this many random cases.
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
      // Make one property per size, but no more than n properties.
        Stream.from(0).take((n min max) + 1) map { i =>
          forAll(g(i))(f)
        }

      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)

      prop.run(max, n, rng)
    }
  }

  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop =
    forAll(n => sgen.g(n))(f)

  def run(prop: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100,
           rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def badCheckImpl(p: => Boolean): Prop = {
    // result is memorized to avoid re-computation
    lazy val result = p
    forAll(Gen.unit(()))(_ => result)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[+A](sample: State[RNG, A]) {
  // exercise 8.6
  def map[B](f: A => B): Gen[B] = Gen(sample map f)
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  // exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  // exercise 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(i => i % 2 == 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // exercise 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  // exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Weight = g1._2 / (g1._2 + g2._2)

    // RNG.double is a 'Double' between [0, 1)
    Gen(State(RNG.double)) flatMap { d => if (d < g1Weight) g1._1 else g2._1 }
  }

  // exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  // exercise 8.13
  def nonEmptyListOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))
}

// We want SGen[A] could be used wherever a Gen[A] is expected, thus covariant on A
case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int) = g(n)

  // exercise 8.11
  def map[B](f: A => B): SGen[B] =
    SGen(g andThen (gen => gen map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(g andThen (_ flatMap f))
}

object Example extends App {
  val smallInt = Gen.choose(-10, 10)

//  Prop.run(Prop.forAll(Gen.listOf(smallInt)) { ns =>
//    // List.max will throw on empty list
//    val max = ns.max
//    !ns.exists(_ > max)
//  })

  Prop.run(Prop.forAll(Gen.nonEmptyListOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  })

  // exercise 8.14
  val sortedProp = forAll(Gen.listOf(smallInt)) { ns =>
    (ns.isEmpty ||
      (ns.sorted.foldLeft((true, Int.MinValue)) { case ((flag, cur), next) =>
        (flag && (cur <= next), next)
      })._1) &&
      ns.forall(ns.sorted.contains(_)) &&
      ns.sorted.forall(ns.contains(_))
  }
  Prop.run(sortedProp)

  // although the result is memorized inside 'check', the test runner will still generate multiple test cases
  // (due to the use of 'forAll')
  // Prop.run(Prop.badCheckImpl(true))
  Prop.run(Prop.check(true))
}

