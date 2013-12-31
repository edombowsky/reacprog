package kvstore

import akka.actor.{Props, Actor, ActorRef, ReceiveTimeout}
import scala.concurrent.duration._
import scala.language.postfixOps
import Persistence._

object PersistenceAgent {

  case class FailedPersistence(key: String, id: Long)

  def props[A](requestor: ActorRef, persistence: ActorRef, msg: Persist, response: (String, Long) => A) = 
    Props(classOf[PersistenceAgent[A]], requestor, persistence, msg, response)
}

class PersistenceAgent[A](requestor: ActorRef, persistence: ActorRef, msg: Persist, response: (String, Long) => A) extends Actor {
  import PersistenceAgent._
  import context.dispatcher

  context.setReceiveTimeout(1 seconds)

  val handle = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds, persistence, msg)

  def receive = {
    case Persisted(key, seq) =>
      handle.cancel()
      requestor ! response(key, seq)

    case ReceiveTimeout => requestor ! FailedPersistence(msg.key, msg.id)
  }

}
