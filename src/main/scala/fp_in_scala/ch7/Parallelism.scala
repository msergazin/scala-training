package fp_in_scala.ch7

import java.util.concurrent._

import fp_in_scala.ch7.Par.Par

import scala.concurrent.duration.TimeUnit
//case class Par[A](a: A, isShutDown: Boolean, marked: Boolean, isTerminated: Boolean)


object Par {
  type Par[A] = ExecutorService => Future[A]
  /**
   * lazyUnit wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
   * */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  /**
   * run extracts a value from a Par by actually performing the computation
   *
   * Fully evaluates a given Par, spawning
   * parallel computations as requested by
   * fork and extracting the resulting value.
   * */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  /**
   * unit promotes a constant value to a parallel computation
   *
   * unit is represented as a function that returns a UnitFuture, which is a
   * simple implementation of Future that just wraps a constant value. It doesn’t
   * use the ExecutorService at all. It’s always done and can’t be cancelled. Its
   * get method simply returns the value that we gave it.
   * */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  /*def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }*/
  /**
   * This version respects timeouts. See `Map2Future` below.
   * map2 combines the results of two parallel computations with a binary function
   * */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }
  /**
   * Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
   * We could prevent this using synchronization, but it isn't needed for our purposes here
   * (also, repeated evaluation of pure values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }
  /**
   * using lazyUnit, write a function to convert any function A => B to one that evaluates its
   * result asynchronously.
   *
   * asyncF converts an A => B to an A => Par[B] by forking a parallel computation
   * to produce the result.
   * */
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }
  /**
   * fork marks a computation for concurrent evaluation. The evaluation won’t
   * actually occur until forced by run.
   * */
  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }
  /**
   * Would run hugeComputation in the main thread, which is exactly what we wanted to avoid by calling fork.
   * It lets us delay instantiation of a computation until it’s actually needed.
   * */
  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    /*map2(parList, unit(()))((a,_) => a.sorted)*/
    map(parList)(_.sorted)
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))
  /*map over a list in parallel
  *needs to combine N parallel computations*/
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  /**
   * Implement parFilter, which filters elements of a list in parallel.
   * */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get
}

object Parallelism extends App {
//  Par.asyncF((_: Int) + 2)
  import Par._
//  assert(map(unit(1))(_ + 1) == unit(2))
  val a = lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(Par.equal(S)(a, fork(a)))
}
