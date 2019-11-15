package manning.ch6_purely_func_state

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}
object RNG  extends App {
  type Rand[+A] = RNG => (A, RNG)
  val int:Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
  //  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
  def double_1: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i ,r2) = rng.nextInt
    val (d, r3) = double(r2)
    ((i,d), r3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r2) = intDouble(rng)
    ((d,i),r2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1,d2,d3),r4)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n <= 0) (list, rng)
      else {
        val (i,r1) = rng.nextInt
        go(n - 1, i :: list, r1)
      }
    }
    go(count, List(), rng)
  }
  def nonNegativeEven: Rand[Int] = {
    /*rng =>
      map(nonNegativeInt)(i => i - i % 2)(rng)*/ //same as
    map(nonNegativeInt)(i => i - i % 2)
  }
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
    /*rng =>{
      val (v1, r1) = ra(rng)
      val (v2, r2) = rb(r1)
      (f(v1,v2), r2)
    }*/
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = {
    map2(ra, rb)((_, _))
  }
  val randIntDouble: Rand[(Int, Double)] = {
    both(int, double)
  }
  val randDoubleInt: Rand[(Double, Int)] = {
      both(double, int)
    }
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }

  val list = List(1,2,3)
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  assert(n1 == 16159453)
  val (n2, rng3) = rng2.nextInt
  assert(n2 == -1281479697)
  val (n3, rng4) = nonNegativeInt(rng3)
  assert(n3 == 340305901)
  assert(ints(2)(rng)._1 == List(-1281479697, 16159453))
  assert(int(rng)._1 == 16159453)
  assert(unit(1)(rng) == (1,SimpleRNG(42))) //rng is not used
  assert(nonNegativeEven(rng)._1 == 16159452)
  assert(double_1(rng)._1 == 0.007524831686168909)
  assert(randIntDouble(rng)._1 == (16159453,0.5967354848980904))
}
