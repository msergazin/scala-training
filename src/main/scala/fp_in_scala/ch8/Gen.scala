package fp_in_scala.ch8

import fp_in_scala.ch6_purely_func_state.{RNG, SimpleRNG, State}
import fp_in_scala.ch8.Prop._



object Prop {
  /*Gen[A] was something that knows how to generate values of type A*/
  type SuccessCount = Int
  type FailedCase = String
//  def listOf[A](a: Gen[A]): Gen[List[A]]
//  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
//  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
//  forAll(intList)(ns => ns.reverse.reverse == ns)
}
case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))
}

trait Prop {
//  def &&(p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

/*case class State[S, +A](run: S => (A, S)) {*/
object Gen extends App {
  /*It should generate integers in the range start to stopExclusive.*/
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**Implement union, for combining two generators of the
   *  same type into one, by pulling
   *  values from each generator with equal likelihood.*/
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  val rng = SimpleRNG(42)
//  println(choose(1,30).sample)
  println(choose(1,3).sample.run.apply(rng))
  println(union(unit(1), unit(2)))
  println(union(unit(1), unit(6)).sample.run.apply(rng))
}