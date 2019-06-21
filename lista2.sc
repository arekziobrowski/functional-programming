//Arkadiusz Ziobrowski - 229728
import scala.math

//Zadanie 1
def take[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case h::t if n > 0 => h::take(n - 1, t)
    case _ => Nil
  }

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(1, List()) == Nil

//Zadanie 2
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case _::t if n > 0 => drop(n - 1, t)
    case _ => xs
  }

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(1, List()) == Nil
drop(-1, List()) == Nil

//Zadanie 3
def reverse[A](xs: List[A]): List[A] = {
  def reverseInner(xs: List[A], acc:List[A]): List[A] =
    xs match {
      case h::t => reverseInner(t, h::acc)
      case Nil => acc
    }

  reverseInner(xs, Nil)
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(List("Ala")) == List("Ala")
reverse(List()) == List()

//Zadanie 4
def replicate(xs: List[Int]): List[Int] = {
  def replicateInner(x: Int, acc: List[Int], counter: Int): List[Int] =
    if(counter <= 0)
      acc
    else
      replicateInner(x, x::acc, counter - 1)

  xs match {
    case h::t => replicateInner(h, Nil, h):::replicate(t)
    case _ => Nil
  }
}

replicate(List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List(1,1,1)) == List(1,1,1)
replicate(List()) == List()
replicate(List(-1,-2)) == List()

//Zadanie 5
def root3(a: Double): Double = {
  val e = 10e-15
  def root3Inner(x: Double): Double =
    if(math.abs((x * x * x) - a) <= e * math.abs(a))
      x
    else
      root3Inner(x + (a / (x * x) - x) / 3)

  root3Inner(if(a > 1) a / 3 else a)
}

root3(-8.0) == -2.0
root3(8.0) == 2.0
root3(0.0) == 0.0