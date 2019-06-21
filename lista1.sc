//Arkadiusz Ziobrowski - 229728


//Zadanie 1
val suma : List[Double] => Double = (xs:List[Double]) =>
    if(xs == Nil)
        0.0
    else
        xs.head + suma(xs.tail)

/*def suma(xs:List[Double]):Double =
  if(xs == Nil)
    0.0
  else
    xs.head + suma(xs.tail)*/

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6


//Zadanie 2
def last[A](xs:List[A]):A =
    if(xs == Nil)
        throw new Exception("List cannot be empty")
    else if(xs.tail == Nil)
        xs.head
    else
        last(xs.tail)

def ends[A](xs:List[A]):(A,A) =
    if(xs == Nil)
        throw new Exception("List cannot be empty")
    else
        (xs.head, last(xs))

ends(List(1,2,3,5)) == (1,5)
ends(List(1,2)) == (1,2)
ends(List("a")) == ("a", "a")
ends(List())


//Zadanie 3
def posortowana(xs:List[Int]):Boolean =
    if(xs == Nil || xs.tail == Nil)
        true
    else if(xs.head > xs.tail.head)
        false
    else
        posortowana(xs.tail)

posortowana(List()) == true
posortowana(List(1,3,3,5,6,7)) == true
posortowana(List(1,1,1)) == true
posortowana(List(3,2)) == false
posortowana(List(3,2,1,0)) == false


//Zadanie 4
def glue(xs:List[String], sep:String):String =
    if(xs == Nil)
        ""
    else if(xs.tail == Nil)
        xs.head
    else
        xs.head + sep + glue(xs.tail, sep)

glue(Nil, "-") == ""
glue(List("To"), "-") == "To"
glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
