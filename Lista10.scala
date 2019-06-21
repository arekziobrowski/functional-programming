// Arkadiusz Ziobrowski - 229728
import java.util.concurrent.{ArrayBlockingQueue, Semaphore}

import scala.concurrent.ExecutionContext
import scala.util.Random

class Producer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run: Unit =
    for (i <- 1 to 10) {println(s"$getName producing $i"); buf.put(i)}
}
class Consumer(name: String, buf: ArrayBlockingQueue[Int]) extends Thread(name) {
  override def run =
    for (_ <- 1 to 10) println(s"$getName consumed ${buf.take}")
}

object Zad1a extends App {
  val buf = new ArrayBlockingQueue[Int](5)
  new Producer("Producer", buf).start()
  new Consumer("Consumer", buf).start()
}

object Zad1b extends App {
  val buf = new ArrayBlockingQueue[Int](5)
  val pNumber = 2
  val cNumber = 3
  for(i <- 1 to pNumber) new Producer("Producer" + i, buf).start()
  for(i <- 1 to cNumber) new Consumer("Consumer" + i, buf).start()
}

object Zad1c extends App {
  val buf = new ArrayBlockingQueue[Int](5)
  val pNumber = 2
  val cNumber = 3
  for(i <- 1 to pNumber) ExecutionContext.global.execute(() => {
    for (j <- 1 to 10) {println(s"ProducerContext-$i producing $j"); buf.put(j)}
  })
  for(i <- 1 to cNumber) ExecutionContext.global.execute(() => {
    for (_ <- 1 to 10) println(s"ConsumerContext-$i consumed ${buf.take}")
  })
  Thread.sleep(4000)
}

object Zad2 extends App {
  class Table(private val placesNumber: Int = 4) {
    private[this] val places = new Semaphore(placesNumber, true)
    private[this] val chopsticks = new Array[Semaphore](placesNumber + 1)
    for(i <- 0 to placesNumber) chopsticks(i) = new Semaphore(1, true)

    def placePhilosopher(p: Philosopher): Unit = {
      places.acquire()
      println(s"Philosopher-" + p.id + " is trying to take the left chopstick.")
      val chopstickLeft = chopsticks(p.id)
      chopstickLeft.acquire()
      println(s"Philosopher-" + p.id + " is trying to take the right chopstick.")
      val chopstickRight = chopsticks((p.id + 1) % chopsticks.length)
      chopstickRight.acquire()
      println(s"Philosopher-" + p.id + " is eating.")
      Thread.sleep(Random.nextInt(3000))
      println(s"Philosopher-" + p.id + " finished eating.")
      chopstickLeft.release()
      chopstickRight.release()
      places.release()
    }
  }

  class Philosopher(val id: Int, val table: Table) extends Thread("Philosopher-" + id) {
    override def run(): Unit = {
      for(_ <- 0 to 3) {
        println(s"Philosopher-" + id + " is meditating")
        Thread.sleep(Random.nextInt(3000))
        eat()
      }
    }

    def eat() = {
      println(s"Philosopher-" + id + " is going to the dining room.")
      table.placePhilosopher(this)
      println(s"Philosopher-" + id + " is going back to the meditating.")
    }
  }

  val philosophersNumber = 5
  val table = new Table(philosophersNumber - 1)
  val philosophers = new Array[Philosopher](philosophersNumber)
  for(i <- 0 until philosophersNumber) new Philosopher(i, table).start()
}

