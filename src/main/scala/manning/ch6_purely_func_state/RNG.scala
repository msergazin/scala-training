package manning.ch6_purely_func_state
trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def _double(rng: RNG): Rand[Double]
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double,Double,Double), RNG)
  def ints(count: Int)(rng: RNG): (List[Int], RNG)
  def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B]
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C]
}
