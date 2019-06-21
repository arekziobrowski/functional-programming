// Arkadiusz Ziobrowski - 229728

class MyQueue[+A] private (private val in: List[A], private val out: List[A]) {

  def this() = {
    this(Nil, Nil)
  }

  def isEmpty = in.isEmpty && out.isEmpty
  def enqueue[B >: A](x: B) =
    if(out.isEmpty)
      new MyQueue(in, x :: out)
    else
      new MyQueue(x :: in, out)
  def dequeue =
    out match {
      case _ :: Nil => new MyQueue(Nil, in.reverse)
      case _ :: xs => new MyQueue(in, xs)
      case Nil =>
        if(in.isEmpty)
          MyQueue.empty
        else
          new MyQueue(Nil, in.reverse.tail)
    }

  def first =
    out match {
      case x :: _ => x
      case Nil =>
        if(in.isEmpty)
          throw new NoSuchElementException("Queue is empty.")
        else
          in.reverse.head
    }

  def firstOption =
    out match {
      case x :: _ => Some(x)
      case Nil =>
        if(in.isEmpty)
          None
        else
          Some(in.reverse.head)
    }

  override def equals(o: Any): Boolean = {
    val q: MyQueue[A] = o.asInstanceOf[MyQueue[A]]
    q.in == this.in && q.out == this.out
  }
}

object MyQueue {
  def apply[A](xs: A*) = new MyQueue[A](Nil, xs.toList)
  def empty[A] = new MyQueue[A](Nil, Nil)
}

sealed trait BT[A]
case class Empty[A]() extends BT[A]
case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

object Lista7 {

  def breadthBT[A](tree: BT[A]): List[A] = {
    def breadthInner(queue: MyQueue[BT[A]]): List[A] =
      if(queue.isEmpty)
        Nil
      else {
        queue.first match {
          case Node(elem, l, r) => elem :: breadthInner(queue.dequeue.enqueue(l).enqueue(r))
          case Empty() => breadthInner(queue.dequeue)
        }
      }

    breadthInner(MyQueue(tree))
  }

  def main(args: Array[String]): Unit = {
    val q = MyQueue()
    val q1 = MyQueue(1, 2, 3)

    assert(q1.first == 1)
    assert(q1.firstOption == Some(1))

    val q2 = new MyQueue
    //assert(q2.first)
    assert(q2.firstOption == None)

    // isEmpty
    assert(MyQueue.empty.isEmpty)
    assert(q.isEmpty)
    assert(!q.enqueue(2).isEmpty)

    // dequeue
    assert(MyQueue.empty.enqueue(1).dequeue == MyQueue.empty)
    assert(q.enqueue(2).enqueue(1).dequeue == q.enqueue(2).dequeue.enqueue(1))
    assert(MyQueue.empty.dequeue == MyQueue.empty)

    // first
    try {
      MyQueue.empty.first
      assert(false)
    }
    catch {
      case e: NoSuchElementException => assert(true)
      case _ => assert(false)
    }
    assert(q.enqueue(1).first == 1)
    assert(q.enqueue(2).enqueue(1).first == q.enqueue(2).first)

    // firstOption
    assert(q.enqueue(2).enqueue(1).firstOption == q.enqueue(2).firstOption)
    assert(MyQueue.empty.enqueue(1).firstOption == Some(1))
    assert(MyQueue.empty.firstOption == None)

    val t = Node(1, Node(2, Empty(), Empty()), Node(3, Empty(), Node(4, Empty(), Empty())))
    assert(breadthBT(t) == List(1, 2, 3, 4))
    val tt = Node(1,
      Node(2,
        Node(4,
          Empty(),
          Empty()
        ),
        Empty()
      ),
      Node(3,
        Node(5,
          Empty(),
          Node(6,
            Empty(),
            Empty()
          )
        ),
        Empty()
      )
    )
    assert(breadthBT(tt) == List(1, 2, 3, 4, 5, 6))
  }
}

