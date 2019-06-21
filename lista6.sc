//Arkadiusz Ziobrowski 229728

//Zadanie 1

def whileLoop(condition: =>Boolean)(expr: =>Any): Any =
  if(condition) {
    expr
    whileLoop(condition)(expr)
  }

var count = 0
whileLoop (count < 5) {
  println(count)
  count += 1
}

//Zadanie 2

def lrepeat[A](k: Int)(stream: Stream[A]): Stream[A] = {
  def lrepeatInner(elem: A): Stream[A] = {
    lazy val rep: Stream[A] = elem #:: rep
    rep.take(k)
  }
  stream match {
    case p #:: t => lrepeatInner(p) #::: lrepeat(k)(t)
    case Stream.Empty => Stream.Empty
  }
}

lrepeat(1)(Stream.Empty).toList == List()
lrepeat (1) (1 #:: 2 #:: 3 #:: Stream.Empty).toList == List(1, 2, 3)
(lrepeat (3) (Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)

//Zadanie 3

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//a

def lBreath[A](ltree: lBT[A]): Stream[A] = {
  def lBreadthInner(queue: List[lBT[A]]): Stream[A] = {
    queue match {
      case LNode(elem, left, right) :: t =>
        elem #:: lBreadthInner(t ::: List(left(), right()))
      case LEmpty :: t => lBreadthInner(t)
      case Nil => Stream.Empty
    }
  }

  lBreadthInner(List(ltree))
}

//b

def lTree(n: Int): lBT[Int] =
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))


val empty = LEmpty
val tree = LNode[Int](1, () => LNode(2, () => LEmpty, () => LEmpty), () => LNode(3, () => LEmpty, () => LEmpty))

lBreath(empty).toList == List()
lBreath(tree).toList == List(1, 2, 3)

lBreath(LEmpty).toList == List()
lBreath(lTree(1)).take(1).toList == List(1)
lBreath(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

