//package fp_in_scala.ch7.parallelism
//
//import java.util.concurrent.atomic.AtomicReference
//import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
//
//import akka.actor.Actor
//
//
//object Parallelism extends App {
//  sealed trait Future[A] {
//    private[parallelism] def apply(k: A => Unit): Unit
//  }
//  type Par[+A] = ExecutorService => Future[A]
//  def run[A](es: ExecutorService)(p: Par[A]): A = {
//    val ref = new AtomicReference[A]
//    val latch = new CountDownLatch(1)
//    p(es) { a => ref.set(a); latch.countDown }
//    latch.await
//    ref.get
//  }
//
//  def unit[A](a: A): Par[A] =
//    es => new Future[A] {
//      def apply(cb: A => Unit): Unit =
//        cb(a)
//    }
//
//  def fork[A](a: => Par[A]): Par[A] =
//    es => new Future[A] {
//      def apply(cb: A => Unit): Unit =
//        eval(es)(a(es)(cb))
//    }
//  def eval(es: ExecutorService)(r: => Unit): Unit =
//    es.submit(new Callable[Unit] { def call = r })
//
////  val S = Executors.newFixedThreadPool(4)
////  val echoer = Actor[String](S) {
////    msg => println (s"Got message: '$msg'")
////  }
//}
