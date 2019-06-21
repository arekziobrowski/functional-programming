//Arkadiusz Ziobrowski - 229728

//Zadanie 1

//a
def existsRec[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match {
    case h::t => p(h) || existsRec(t)(p)
    case Nil => false
  }

existsRec(List(5,1,2,3)) (_ == 2) == true
existsRec(List()) (_ == 2) == false
existsRec(List(1,1,1,1)) (_ > 2) == false
existsRec(List("a","a","a","a")) (_ == "a") == true

//b
def existsFL[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldLeft false) ((acc, x) => p(x) || acc)

existsFL(List(5,1,2,3)) (_ == 2) == true
existsFL(List()) (_ == 2) == false
existsFL(List(1,1,1,1)) (_ > 2) == false
existsFL(List("a","a","a","a")) (_ == "a") == true

//c
def existsFR[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldRight false) ((x, acc) => acc || p(x))

existsFR(List(5,1,2,3)) (_ == 2) == true
existsFR(List()) (_ == 2) == false
existsFR(List(1,1,1,1)) (_ > 2) == false
existsFR(List("a","a","a","a")) (_ == "a") == true

//Zadanie 2
def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  (xs foldRight List[A]()) ((x, acc) => if(p(x)) x::acc else acc)

filter(List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7,7,8,4,6,9)
filter(List("A", "B")) (_ == "X") == List()
filter(List()) (_ == 1) == List()
filter(List(1,1,1,1)) (_ == 1) == List(1,1,1,1)

//Zadanie 3

//a
def remove1Rec[A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h::t => if(p(h)) t else h::remove1Rec(t)(p)
    case Nil => Nil
  }

remove1Rec(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1Rec(List()) (_ != 3) == List()
remove1Rec(List("a","b","c")) (_ == "d") == List("a","b","c")

//b
def remove1TailRec[A](xs: List[A])(p: A => Boolean): List[A] = {
  def inner(xs: List[A], acc: List[A]): List[A] =
    xs match {
      case h::t => if(p(h)) acc reverse_::: t else inner(t, h::acc)
      case Nil => acc.reverse
    }

  inner(xs, List())
}

remove1TailRec(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1TailRec(List()) (_ != 3) == List()
remove1TailRec(List("a","b","c")) (_ == "d") == List("a","b","c")


//Zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) =
  xs match {
    case h::t =>
      if(n > 0) {
        val tuple = splitAt(t)(n - 1)
        (h :: tuple._1, tuple._2)
      }
      else
        (Nil, xs)
    case Nil => (Nil, Nil)
  }

splitAt (List('a','b','c','d','e')) (2) == (List('a','b'), List('c','d','e'))
splitAt (List('a','b')) (3) == (List('a','b'), List())
splitAt (List('a','b','c','d','e')) (0) == (List(), List('a','b','c','d','e'))
splitAt (List()) (0) == (List(), List())
splitAt (List()) (10) == (List(), List())








