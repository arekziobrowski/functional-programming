// Arkadiusz Ziobrowski - 229728

import scala.reflect.ClassTag

class FullException(msg: String) extends Exception(msg)

abstract class MyQueue[E] {
  @throws[FullException]
  def enqueue(x: E): Unit
  def dequeue: Unit
  @throws[NoSuchElementException]
  def first: E
  def isEmpty: Boolean
  def isFull: Boolean
}

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueue[E] {

  private[this] var f: Int = 0
  private[this] var r: Int = 0
  private[this] var arr = new Array[E](capacity + 1)
  private[this] val modulo: Int = capacity + 1

  override def enqueue(x: E): Unit = {
    if (isFull)
      throw new FullException("Queue is full.")

    arr(r) = x
    r = (r + 1) % modulo
  }

  override def dequeue: Unit =
    if(!isEmpty) {
      arr(f) = null.asInstanceOf[E]
      f = (f + 1) % modulo
    }

  override def first: E =
    if (isEmpty)
      throw new NoSuchElementException("Queue is empty.")
    else
      arr(f)

  override def isEmpty: Boolean = f == r

  override def isFull: Boolean = (r + 1) % modulo == f
}

object QueueMut {
  def apply[E: ClassTag](xs: E*): QueueMut[E] = xs.foldLeft(new QueueMut[E](xs.length))(
    (queue, elem) => {queue.enqueue(elem); queue})
  def empty[E: ClassTag](capacity: Int = 1000) : QueueMut[E] = new QueueMut[E](capacity)
}

object Lista8 {
  def main(args: Array[String]): Unit = {
    assert(QueueMut(1, 2, 3).first == 1)

    var queue = QueueMut("aa", "bc", "de")
    queue.dequeue
    assert(!queue.isEmpty)
    assert(queue.first == "bc")

    var q = new QueueMut[Int](1)
    assert(q.isEmpty)

    q.dequeue
    assert(q.isEmpty)

    try {
      q.enqueue(1)
      q.dequeue
      assert(q.isEmpty)

      q.enqueue(1)
      assert(q.isFull)

      q.dequeue
      q.enqueue(11)
      assert(q.first == 11)
      assert(q.isFull)

      q.enqueue(2)
      assert(false)
    }
    catch {
      case e: FullException => assert(true)
      case _ => assert(false)
    }

    try {
      QueueMut.empty().first
      assert(false)
    }
    catch {
      case e: NoSuchElementException => assert(true)
      case _ => assert(false)
    }


  }
}
