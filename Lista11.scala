// Arkadiusz Ziobrowski - 229728

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success}

object Zad1a extends App {
  def pairFut[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2

  val f1 = Future { Thread.sleep(2000); 1 }
  val f2 = Future { Thread.sleep(1000); "a" }

  val result = Await.result(pairFut(f1, f2), 3.seconds)
  println(s"Result: $result")
}

object Zad1b extends App {
  def pairFut[A, B] (fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    for {
      x <- fut1
      y <- fut2
    } yield (x, y)

  val f1 = Future { Thread.sleep(2000); 1 }
  val f2 = Future { Thread.sleep(1000); "a" }

  val result = Await.result(pairFut(f1, f2), 3.seconds)
  println(s"Result: $result")
}

object Zad2a extends App {
  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] = {
      val prom = Promise[Boolean]
      self map p onComplete {
        case Success(true) => prom trySuccess true
        case _ => prom trySuccess false
      }
      prom.future
    }
  }

  val f1 = Future { Thread.sleep(1000); 1 } exists(x => true)
  val r1 = Await.result(f1, 3.seconds)
  println(s"Result: $r1")

  val f2 = Future { Thread.sleep(1000); 1 / 0 } exists(_ => true)
  val r2 = Await.result(f2, 3.seconds)
  println(s"Result: $r2")

  val f3 = Future { Thread.sleep(1000); 1 } exists(_ => false)
  val r3 = Await.result(f3, 3.seconds)
  println(s"Result: $r3")
}

object Zad2b extends App {
  implicit class FutureOps[T](val self: Future[T]) {
    def exists(p: T => Boolean): Future[Boolean] =
      self map p recover {
        case _ => false
      }
  }

  val f1 = Future { Thread.sleep(1000); 1 } exists(x => x > 0)
  val r1 = Await.result(f1, 3.seconds)
  println(s"Result: $r1")

  val f2 = Future { Thread.sleep(1000); 1 / 0 } exists(x => x > 0)
  val r2 = Await.result(f2, 3.seconds)
  println(s"Result: $r2")

  val f3 = Future { Thread.sleep(1000); -1 } exists(x => x > 0)
  val r3 = Await.result(f3, 3.seconds)
  println(s"Result: $r3")
}

object WordCount {
  def main(args: Array[String]) {
    val path = "/home/arek/Desktop/scala-test/"
    val promiseOfFinalResult = Promise[Seq[(String, Int)]]
    // Tu oblicz promiseOfFinalResult
    promiseOfFinalResult tryCompleteWith (scanFiles(path) flatMap (fileNames => processFiles(fileNames)))
    promiseOfFinalResult.future onComplete {
      case Success(result) => result.sortBy(_._2) foreach println
      case Failure(t) => t.printStackTrace()
    }
    Thread.sleep(5000)
  }

  // Oblicza liczbę słów w każdym pliku z sekwencji wejściowej
  private def processFiles(fileNames: Seq[String]): Future[Seq[(String, Int)]] =
    Future.sequence(for(filename <- fileNames) yield processFile(filename))
  // Wskazówka. Wykorzystaj Future.sequence(futures)
  // Oblicza liczbę słów w podanym pliku i zwraca parę: (nazwa pliku, liczba słów)
  private def processFile(fileName: String): Future[(String, Int)] = Future {
    val f = Source.fromFile(fileName)
    try { (fileName, f.getLines.mkString("\n").split("\\s+").length) } finally f.close()
  }

  // Zwraca sekwencję nazw plików (w naszym przypadku Array[String])
  private def scanFiles(docRoot: String): Future[Seq[String]] =
    Future { new java.io.File(docRoot).list.map(docRoot + _) }
}
