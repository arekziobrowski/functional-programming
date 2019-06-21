//Arkadiusz Ziobrowski - 229728

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.io.StdIn
import scala.util.Random

class Server(N: Int) extends Actor {
  private val number = new Random nextInt N

  override def receive = {
    case Server.Start => println(s"Guess my number from the interval [0..$N]")
    case Server.M(guess) =>
      if (guess > number)
        sender ! Client.R(1, guess)
      else if (guess < number)
        sender ! Client.R(-1, guess)
      else
        sender ! Client.R(0, guess)
    case msg => sender ! s"Wrong request: '$msg'"
  }
}

object Server {
  def props(N: Int) = Props(classOf[Server], N)

  case class M(guess: Int)

  case object Start

}

class Client(name: String, server: ActorRef, upperLimit: Int) extends Actor {
  private var start = 0
  private var end = upperLimit

  override def receive = {
    case Client.Start => {
      println(s"$name starting")
      val firstGuess = new Random nextInt upperLimit
      println(s"$name trying: $firstGuess")
      server ! Server.M(firstGuess)
    }
    case Client.R(1, guess) => {
      end = guess
      val newGuess = (end - start) / 2 + start
      println(s"$name. Response: too big! Trying: $newGuess")
      server ! Server.M(newGuess)
    }
    case Client.R(-1, guess) => {
      start = guess
      val newGuess = (end - start) / 2 + start
      println(s"$name. Response: too small! Trying: $newGuess")
      server ! Server.M(newGuess)
    }
    case Client.R(0, guess) => {
      println(s"$name: I guessed it! $guess")
      println(s"Goodbye! from $name")
      context.system.terminate
    }
    case other => println(other)
  }
}

object Client {
  def props(name: String, upperLimit: Int) = Props(classOf[Client], name, Main.server, upperLimit)

  case class R(response: Int, guess: Int)

  case object Start

}

object Main extends App {
  // ActorSystem is a heavy object: create only one per application
  val ourSystem = ActorSystem("MySystem")

  println("Enter N:")
  val N = StdIn.readInt()
  val server: ActorRef = ourSystem.actorOf(Server.props(N))
  server ! Server.Start

  for(i <- 0 to 2) {
    val client: ActorRef = ourSystem.actorOf(Client.props("Client" + i, N))
    client ! Client.Start
  }
}
