import scala.collection.immutable.LazyList.cons

val stream3: LazyList[Int] = cons(1, cons(2, cons(3, LazyList.empty) ) )
println(s"Elements of stream3 = ${stream3}")
println(s"3 of stream3 = ${stream3.take(3).force}")