import java.sql.{Date, Time}
//import java.util.Date

def parallSum(ints: IndexedSeq[Int]): Int = {
  if (ints.size <= 1)
    ints.headOption getOrElse 0
  else {
    val (l,r) = ints.splitAt(ints.size / 2)
    parallSum(l) + parallSum(r)
  }
}

val vector = (1 until 10000).toVector
parallSum(vector)

vector.sum


def sum(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1)
    ints.headOption getOrElse 0
else {
  val (l,r) = ints.splitAt(ints.length/2)
  val sumL: Par[Int] = Par.unit(sum(l))
  val sumR: Par[Int] = Par.unit(sum(r))
  Par.get(sumL) + Par.get(sumR)
}

def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(sum(l), sum(r))(_ + _)
  }

def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]

def fork[A](a: => Par[A]): Par[A]

def sum(ints: IndexedSeq[Int]): Par[Int] = {
  if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else
    val (l,r) = ints.splitAt(ints.length / 2)
  Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
}

def run[A](a: Par[A]): A