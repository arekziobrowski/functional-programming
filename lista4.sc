//Arkadiusz Ziobrowski 229728

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val tempty = Empty
val tone = Node(1, Empty, Empty)

val tt = Node(1,
  Node(2,
    Node(4,
      Empty,
      Empty
    ),
    Empty
  ),
  Node(3,
    Node(5,
      Empty,
      Node(6,
        Empty,
        Empty
      )
    ),
    Empty
  )
)

//Zadanie 1

def sumBT(bt: BT[Int]): Int =
  bt match {
    case Node(v, l, r) => v + sumBT(l) + sumBT(r)
    case Empty => 0
  }

sumBT(t) == 6
sumBT(tempty) == 0
sumBT(tt) == 21
sumBT(tone) == 1

//Zadanie 2

def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B =
  bt match {
    case Node(v, l, r) => f(v)(foldBT(f)(acc)(l), foldBT(f)(acc)(r))
    case Empty => acc
  }

//Zadanie 3

//a

def sumBTfold(bt: BT[Int]): Int =
  foldBT((v: Int) => (l: Int, r: Int) => v + l + r)(0)(bt)

sumBTfold(t) == 6
sumBTfold(tempty) == 0
sumBTfold(tt) == 21
sumBTfold(tone) == 1

//b

def inorderBTfold[A](bt: BT[A]): List[A] =
  foldBT((v: A) => (l: List[A], r: List[A]) => l ::: v :: r)(List())(bt)

inorderBTfold(t) == List(2, 3, 1)
inorderBTfold(tempty) == List()
inorderBTfold(tt) == List(4, 2, 1, 5, 6, 3)
inorderBTfold(tone) == List(1)

//Zadanie 4

def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] = {
  foldBT[A, BT[B]](v => (l, r) => Node(f(v), l, r))(Empty)(tree)
}

mapBT((v: Int) => 2 * v)(t: BT[Int]) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)
mapBT((v: Int) => 2 * v)(tone: BT[Int]) == Node(2, Empty, Empty)
mapBT((v: Int) => 2 * v)(tempty: BT[Int]) == Empty
mapBT((v: Int) => 2 * v)(tt: BT[Int]) == Node(2,
  Node(4,
    Node(8,
      Empty,
      Empty
    ),
    Empty
  ),
  Node(6,
    Node(10,
      Empty,
      Node(12,
        Empty,
        Empty
      )
    ),
    Empty
  )
)

//Zadanie 5

sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): Boolean =
    toVisit match {
      case h :: ts =>
        if(h == to)
          true
        else
          if(visited contains h)
            search(visited)(ts)
          else
            search(h :: visited)(ts ::: (g succ h))
      case Nil => false
    }
  search(Nil)(List(from))
}

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0,2,4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0,2)
    case n => throw
      new NoSuchElementException("Graph g: node " + n
        + " doesn't exist")
  })

pathExists(g)(4,1) == true
pathExists(g)(0,4) == false

