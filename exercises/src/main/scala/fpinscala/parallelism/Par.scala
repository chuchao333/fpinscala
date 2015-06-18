package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that
  // just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having
  // `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we
  // want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This
      // means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It
      // simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures
      // `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need
      // a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time
      // from the available time allocated for evaluating `bf`.
      // UnitFuture(f(af.get, bf.get))

      // exercise 3
      new CombinedFuture(af, bf, f)
    }

  private class CombinedFuture[A, B, C](af: Future[A], bf: Future[B], combinator: (A, B) => C) extends Future[C] {
    override def cancel(evenIfRunning: Boolean): Boolean = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(l: Long, timeUnit: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(l, timeUnit))

    override def isDone: Boolean = result.isDefined

    @volatile private var result: Option[C] = None

    private def compute(timeoutMs: Long) = result getOrElse {
      val start = System.currentTimeMillis
      val aRes = af.get(timeoutMs, TimeUnit.MILLISECONDS)
      val aTime = System.currentTimeMillis - start
      val bRes = bf.get(timeoutMs - aTime, TimeUnit.MILLISECONDS)
      val res = combinator(aRes, bRes)
      result = Some(res)
      res
    }
  }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the
  // outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our
  // thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential
  // parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious
  // problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // the latest version of the book call this 'lazyUnit'
  def async[A](a: => A): Par[A] = fork(unit(a))

  // exercise 4
  def asyncF[A, B](f: A => B): A => Par[B] = { (a: A) => async(f(a)) }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // exercise 5 optional (implement 'product' and 'map' as primitives)
  def product[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] =
    (es: ExecutorService) => {
      val fa = pa(es)
      val fb = pb(es)
      new CombinedFuture[A, B, (A, B)](fa, fb, { (f1, f2) => (f1, f2) })
    }

  def map1[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => {
      val fa = pa(es)
      new MapFuture(fa, f)
    }

  class MapFuture[A, B](fa: Future[A], f: A => B) extends Future[B] {
    override def cancel(b: Boolean): Boolean = fa.cancel(b)

    override def isCancelled: Boolean = fa.isCancelled

    override def get(): B = result getOrElse {
      val res = f(fa.get)
      result = Some(res)
      res
    }

    override def get(l: Long, timeUnit: TimeUnit): B = result getOrElse {
      val res = f(fa.get(l, timeUnit))
      result = Some(res)
      res
    }

    override def isDone: Boolean = result.isDefined

    @volatile private var result: Option[B] = None
  }

  // Why we have to use the 'case (a, b)' rather than just '(a, b)' when map over a tuple?
  def map2ViaProductAndMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    map1(product(pa, pb)) { case (a, b) => f(a, b) }

  // exercise 5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(Nil: List[A])) {
    (px, pacc) => map2(px, pacc) { (x, acc) => x :: acc }
  }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = as.map(asyncF(f))
    sequence(fbs)
  }

  // exercise 6
  def sequenceWithFilter[A](ps: List[Par[A]], f: A => Boolean): Par[List[A]] = ps.foldRight(unit(Nil: List[A])) {
    (px, pacc) => map2(px, pacc) { (x, acc) => if (f(x)) x :: acc else acc }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[A]] = as.map(asyncF(x => x))
    sequenceWithFilter(fbs, f)
  }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // exercise 11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val index = n(es).get // Notice we are blocking on the result of 'n'
      choices(index)(es)
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond) { c => if (c) 0 else 1 })(List(t, f))

  // exercise 12
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = key(es).get
      choices(k)(es)
    }

  // exercise 13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = pa(es).get  // Notice we are blocking on the result of 'pa'
      choices(a)(es)
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond) { c => if (c) t else f }

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n) { index => choices(index) }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = chooser(pa)(f)

  // exercise 14
  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val inner = a(es).get
      inner(es)
    }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples {

  import Par._

  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these
  // sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
