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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
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
      if (n <= 0) (List(), rng)
      else {
        val (i,r1) = rng.nextInt
        go(n - 1, i :: list, r1)
      }
    }
    go(count, List(), rng)
  }
  
  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  assert(n1 == 16159453)
  val (n2, rng3) = rng2.nextInt
  assert(n2 == -1281479697)
  val (n3, rng4) = nonNegativeInt(rng3)
  assert(n3 == 340305901)
  println(ints(2)(rng))

}
