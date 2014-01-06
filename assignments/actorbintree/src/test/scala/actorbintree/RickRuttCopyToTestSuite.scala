/**
 * Copyright 2013 Rick Rutt.
 * Usage is allowed for educational purposes provided this copyright notice is preserved.
 */
package actorbintree

import akka.actor.Actor
import akka.actor.{ Props, ActorRef, ActorSystem }
import akka.testkit.{ ImplicitSender, TestKit }
import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

class FosterParent(childProps: Props, tester: ActorRef) extends Actor {
  val child = context.actorOf(childProps, "child")
  def receive = {
    case msg if context.sender == child => {
      tester forward msg
    }
    case msg => {
      child forward msg
    }
  }
}

@RunWith(classOf[JUnitRunner])
class NodeCopyToFosterParentTestSuite(_system: ActorSystem) extends TestKit(_system) with FunSuite with ShouldMatchers with BeforeAndAfterAll with ImplicitSender
{
  def this() = this(ActorSystem("PostponeSpec"))

  override def afterAll: Unit = system.shutdown()

  import actorbintree.BinaryTreeSet._

  test("empty leaf node confirms copy without actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))
  }

  test("active leaf node confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))
  }

  test("empty node with two children confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("active node with two children confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("empty node with left child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("active node with left child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, -1, -1)
    expectMsg(OperationFinished(-1))
    fosterNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = -1, -1)
    expectMsg(ContainsResult(-1, true))
  }

  test("empty node with right child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = true), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, false))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))
  }

  test("active node with right child confirms copy while actually copying to new node") {
    val fosterNode = system.actorOf(Props(classOf[FosterParent], BinaryTreeNode.props(0, initiallyRemoved = false), testActor))

    val newNode = system.actorOf(BinaryTreeNode.props(1, initiallyRemoved = true))

    fosterNode ! Insert(testActor, 1, 1)
    expectMsg(OperationFinished(1))
    fosterNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))

    fosterNode ! BinaryTreeNode.CopyTo(newNode)
    expectMsg(BinaryTreeNode.CopyFinished)

    newNode ! Contains(testActor, id = 0, 0)
    expectMsg(ContainsResult(0, true))

    newNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, true))
  }
}
