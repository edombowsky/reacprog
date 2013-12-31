package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor, Cancellable }
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
  import PersistenceAgent._
  import context.dispatcher
  import scala.language.postfixOps

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
    case _: Exception => SupervisorStrategy.Restart
  }

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  // a persister actor
  val persistence = context.actorOf(persistenceProps)

  // map from request id to pair of sender and cancellable
  var acks = Map.empty[Long, (ActorRef, Cancellable)]

  var persistReq = Map.empty[Long, (ActorRef, Int)]

  var requests = Map.empty[Long,(ActorRef, Int, Cancellable)]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica(0L))
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Replicas(reps) => {
      val newReps = reps -- secondaries.keys
      val deadReps = secondaries.keySet -- reps
      val newSecs = newReps.map(replica => replica -> context.actorOf(Replicator.props(replica))).toMap

      for {
        replicator <- newSecs.values
        (key, value) <- kv
      } replicator ! Replicate(key, Some(value), 0L)

      deadReps.foreach { replica =>
        replica ! PoisonPill
        secondaries(replica) ! PoisonPill
      }

      replicators = replicators ++ newSecs.values -- deadReps
      secondaries = secondaries ++ newSecs -- deadReps
    }

    case Insert(key, value, id) => 
      val cancellable = context.system.scheduler.scheduleOnce(1 second, self, CleanupFailedRequest(id))
      requests = requests + (id -> (sender, 0, cancellable))
      kv += (key -> value)
      replicators.foreach(_ ! Replicate(key, Some(value), id))
      context.actorOf(PersistenceAgent.props(self, persistence, Persist(key, Some(value), id), Replicated.apply _))

    case Remove(key, id) =>
      val cancellable = context.system.scheduler.scheduleOnce(1 second, self, CleanupFailedRequest(id))
      requests = requests + (id -> (sender, 0, cancellable)) 
      kv -= key
      replicators.foreach(_ ! Replicate(key, None, id))
      context.actorOf(PersistenceAgent.props(self, persistence, Persist(key, None, id), Replicated.apply _))

    case Get(key, id) => sender ! GetResult(key, kv get key, id)

    case Replicated(key, id) =>
      requests.get(id).foreach { case (client, acksReceived, cancellable) =>
        if ((acksReceived + 1) >= replicators.size) {
          cancellable.cancel
          requests = requests - id
          client ! OperationAck(id)
        } else {
          requests = requests + (id -> (client, acksReceived + 1, cancellable))
        }
      }

    case FailedPersistence(key, id) =>  
      requests.get(id).foreach { case (client, acksReceived, cancellable) =>
        cancellable.cancel
        requests = requests - id
        client ! OperationFailed(id)
      }

    case CleanupFailedRequest(id) =>
      requests.get(id).foreach { case (client, acksReceived, cancellable) =>
        requests = requests - id
        client ! OperationFailed(id)
      }
  }

  /* TODO Behavior for the replica role. */
  def replica(seqNo: Long): Receive = {
    case Get(key, id) => sender ! GetResult(key, kv get key, id)
    
    case Snapshot(key, _, seq) if seq < seqNo => sender ! SnapshotAck(key, seq)

    case Snapshot(key, valueOption, seq) if seq == seqNo => {
      valueOption match { 
        case Some(value) => kv += (key -> value) 
        case None => kv -= key
      }

      context.actorOf(PersistenceAgent.props(sender, persistence, Persist(key, valueOption, seq), SnapshotAck.apply _))
      context.become(replica(seqNo + 1L))
    }
  }

  override def preStart = {
    arbiter ! Join
  }

}
