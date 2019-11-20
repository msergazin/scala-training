package fp_in_scala.ch4

sealed trait Option[+A] {
  /*Apply f if the Option is not None.*/
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  /*Apply f, which may fail, to the Option if not None.*/
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse(None)
  /*The B >: A says that the B type parameter must be a supertype of A.*/
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  /*Don’t evaluate ob unless needed.*/
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h,t) => map2(h,t)((h,t) => h::t))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//    (a, b) match {
//      case (Some(a), Some(b)) => Some(f(a, b))
//      case _ => None
//    } //works

//    a flatMap (
//      aa => b map (
//        bb => f(aa, bb)
//        )
//      ) //also works - book solution

    for {
      aa <- a
      bb <- b
    } yield f(aa, bb) //for-comprehension solution


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
  }
  def lift[A,B](f: A => B): Option[A] => Option[B] =
    _ map f



  def main(args: Array[String]) = {
//    println(mean(Seq(1,2,3,4)))
//    val absO: Option[Double] => Option[Double] = lift(math.abs)
//    absO(Some(9))

//    println(map2(Some(1), Some(2))(_ + _))
//    println(map2(None, Some(2))((x: Int, y: Int) => x + y))
  }
}
