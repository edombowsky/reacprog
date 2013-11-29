package nodescala

import scala.io.Source
import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import scala.util.{Failure, Success, Try}

object Main {

  def main(args: Array[String]) {
    // TO IMPLEMENT
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)
    // EMD
    val myServerSubscription = myServer.start("/test") {
        (request => for (kv <- request.iterator) yield (kv + "\n").toString)
    }

    // TO IMPLEMENT
    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    // EMD
    val userInterrupted: Future[String] = {
        Future.userInput("Please enter").continue({
          case Success(message) => "You entered... " + message
          case Failure(e) => throw e
        })
    }

    // TO IMPLEMENT
    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    // EMD
    val timeOut: Future[String] = Future.delay(20 seconds).continue(t => "Server timeout!")

    // TO IMPLEMENT
    // 4. create a future that completes when either 10 seconds elapse
    //    or the user enters some text and presses ENTER
    // EMD
    //val terminationRequested: Future[String] = Future.any(List(userInterrupted, timeOut))
    val terminationRequested: Future[String] = {
        Future.delay(10 second).continue(_ => "\n Server Timeout!")
    }

    // TO IMPLEMENT
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg =>
        println(msg)
        myServerSubscription.unsubscribe()
        println("Bye!")
    }
    Await.ready(terminationRequested, 30 seconds)
  }

}
