package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  
  var kv = Map.empty[String, String]
  var expectedSeqs = Map.empty[String, Long]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  def receive = {
    case JoinedPrimary   ⇒ context.become(leader)
    case JoinedSecondary ⇒ context.become(replica)
  }

  // EMD
  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      sender ! OperationAck(id)
    }
    case Remove(key, id) => {
      kv -= key
      sender ! OperationAck(id)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }  
  }

  // EMD
  val replica: Receive = {
    case Snapshot(key, value, seq) => {
      val expected = expectedSeq(key)
      if (seq == expected) update(key, value)
      if (seq <= expected) {
        expectedSeqs += key -> Math.max(seq  + 1, expected)
        sender ! SnapshotAck(key, seq)
      }
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
  }

  // EMD
  def update(key: String, value: Option[String]) {
    if (value.isDefined) {
      kv += key -> value.get
    }
    else {
      kv -= key
    }
  }

  // EMD
  def expectedSeq(key: String): Long = expectedSeqs.get(key).getOrElse(0L)

  // EMD
  override def preStart(): Unit = {
    arbiter ! Join
  }
}