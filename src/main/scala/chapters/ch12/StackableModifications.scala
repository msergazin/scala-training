package chapters.ch12

import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int
  def put(x: Int)
}
class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int) = { buf += x }
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) = { super.put(2 * x) }
}
trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = { super.put(x + 1) }
}
trait Filtering extends IntQueue {
  abstract override def put(x: Int) = {
    if (x >= 0) super.put(x)
  }
}

class MyQueueDoublePut extends BasicIntQueue with Doubling
class MyQueueFilterThenIncrementBy1ThenDoublePut
  extends BasicIntQueue
    with Doubling
    with Incrementing
    with Filtering
class MyQueue extends BasicIntQueue

object StackableModifications extends App {
//  val queue = new MyQueue
//  val queueDouble = new MyQueueDoublePut
//  queue.put(1)
//  queueDouble.put(1)
//  println(queue.get())
//  println(queueDouble.get())
  val queueDoublePutIncrementBy1AndFilter = new MyQueueFilterThenIncrementBy1ThenDoublePut
  queueDoublePutIncrementBy1AndFilter.put(7)
  queueDoublePutIncrementBy1AndFilter.put(-1)
  println(queueDoublePutIncrementBy1AndFilter.get())
}
