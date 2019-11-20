package fp_in_scala.ch7

import java.util.concurrent._

import fp_in_scala.ch7.Par.Par

import scala.concurrent.duration.TimeUnit
//case class Par[A](a: A, isShutDown: Boolean, marked: Boolean, isTerminated: Boolean)


object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  /**
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

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
}

object Parallelism {
  def unit[A](a: A): Par[A]
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
  def fork[A](a: => Par[A]): Par[A]
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](a: Par[A]): A
}
