/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import akka.event.LoggingReceive

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  // EMD
  // val normal: Receive = { case _ => ??? }
  val normal: Receive = {
    case insert: Insert     => root ! insert
    case contains: Contains => root ! contains
    case remove: Remove     => root ! remove
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  // EMD
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case operation: Operation => 
      pendingQueue = pendingQueue :+ operation
    case CopyFinished =>
      root ! PoisonPill
      root = newRoot
      
      pendingQueue.foreach(newRoot ! _)
      pendingQueue = Queue.empty[Operation]
      
      context.unbecome()
    case GC => // ignore
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  // EMD
  val normal: Receive = {
    case insert@Insert(requester, id, value) =>
      if (value < elem) handleInsert(Left, insert)
      else if (value > elem) handleInsert(Right, insert)
      else {
        removed = false
        requester ! OperationFinished(id)
      }

    case contains@Contains(requester, id, value) =>
      if (value < elem) handleContains(Left, contains)
      else if (value > elem) handleContains(Right, contains)
      else requester ! ContainsResult(id, !removed)

    case remove@Remove(requester, id, value) =>
      if (value < elem) handleRemove(Left, remove)
      else if (value > elem) handleRemove(Right, remove)
      else {
        removed = true
        requester ! OperationFinished(id)
      }
      
    case copy@CopyTo(newRoot) => 
      val children = subtrees.values.toSet
      if(children.isEmpty && removed) context.parent ! CopyFinished
      else {
        if (!removed) newRoot ! Insert(self, elem, elem)
        children.foreach(_ ! copy)
        context.become(copying(children, insertConfirmed = removed))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  // EMD
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      if (expected.isEmpty) context.parent ! CopyFinished
      context.become(copying(expected, insertConfirmed = true))

    case CopyFinished =>
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) context.parent ! CopyFinished
      context.become(copying(newExpected, insertConfirmed))
  }

  // EMD
  private def handleInsert(position: Position, insert: Insert) = subtrees.get(position) match {
    case Some(actor) => actor ! insert

    case None =>
      subtrees += position -> context.actorOf(BinaryTreeNode.props(insert.elem, initiallyRemoved = false))
      insert.requester ! OperationFinished(insert.id)
  }

  // EMD
  private def handleContains(position: Position, contains: Contains) = subtrees.get(position) match {
    case Some(actor) => actor ! contains

    case None => contains.requester ! ContainsResult(contains.id, result = false)
  }

  // EMD
  private def handleRemove(position: Position, remove: Remove) = subtrees.get(position) match {
    case Some(actor) => actor ! remove
    
    case None => remove.requester ! OperationFinished(remove.id)
  }


}
