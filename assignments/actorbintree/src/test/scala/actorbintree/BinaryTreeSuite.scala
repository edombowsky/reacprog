/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.{ Props, ActorRef, ActorSystem }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import scala.concurrent.duration._
import org.scalatest.FunSuite


class BinaryTreeSuite(_system: ActorSystem) extends TestKit(_system) with FunSuite with ShouldMatchers with BeforeAndAfterAll with ImplicitSender 
{

  def this() = this(ActorSystem("PostponeSpec"))

  override def afterAll: Unit = system.shutdown()

  import actorbintree.BinaryTreeSet._

  def receiveN(requester: TestProbe, ops: Seq[Operation], expectedReplies: Seq[OperationReply]): Unit =
    within(5.seconds) {
      val repliesUnsorted = for (i <- 1 to ops.size) yield try {
        requester.expectMsgType[OperationReply]
      } catch {
        case ex: Throwable if ops.size > 10 => fail(s"failure to receive confirmation $i/${ops.size}", ex)
        case ex: Throwable                  => fail(s"failure to receive confirmation $i/${ops.size}\nRequests:" + ops.mkString("\n    ", "\n     ", ""), ex)
      }
      val replies = repliesUnsorted.sortBy(_.id)
      if (replies != expectedReplies) {
        val pairs = (replies zip expectedReplies).zipWithIndex filter (x => x._1._1 != x._1._2)
        fail("unexpected replies:" + pairs.map(x => s"at index ${x._2}: got ${x._1._1}, expected ${x._1._2}").mkString("\n    ", "\n    ", ""))
      }
    }

  def verify(probe: TestProbe, ops: Seq[Operation], expected: Seq[OperationReply]): Unit = {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    ops foreach { op =>
      topNode ! op
    }

    receiveN(probe, ops, expected)
  }

  test("proper inserts and lookups") {
    val topNode = system.actorOf(Props[BinaryTreeSet])

    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))

    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)

    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
  }

  test("instruction example") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, id=100, 1),
      Contains(requesterRef, id=50, 2),
      Remove(requesterRef, id=10, 1),
      Insert(requesterRef, id=20, 2),
      Contains(requesterRef, id=80, 1),
      Contains(requesterRef, id=70, 2)
      )
   
    val expectedReplies = List(
      OperationFinished(id=10),
      OperationFinished(id=20),
      ContainsResult(id=50, false),
      ContainsResult(id=70, true),
      ContainsResult(id=80, false),
      OperationFinished(id=100)     
      )

    verify(requester, ops, expectedReplies)
  }
  
  test("behave identically to built-in set (includes GC)") {
    val rnd = new Random()
    def randomOperations(requester: ActorRef, count: Int): Seq[Operation] = {
      def randomElement: Int = rnd.nextInt(100)
      def randomOperation(requester: ActorRef, id: Int): Operation = rnd.nextInt(4) match {
        case 0 => Insert(requester, id, randomElement)
        case 1 => Insert(requester, id, randomElement)
        case 2 => Contains(requester, id, randomElement)
        case 3 => Remove(requester, id, randomElement)
      }

      for (seq <- 0 until count) yield randomOperation(requester, seq)
    }

    def referenceReplies(operations: Seq[Operation]): Seq[OperationReply] = {
      var referenceSet = Set.empty[Int]
      def replyFor(op: Operation): OperationReply = op match {
        case Insert(_, seq, elem) =>
          referenceSet = referenceSet + elem
          OperationFinished(seq)
        case Remove(_, seq, elem) =>
          referenceSet = referenceSet - elem
          OperationFinished(seq)
        case Contains(_, seq, elem) =>
          ContainsResult(seq, referenceSet(elem))
      }

      for (op <- operations) yield replyFor(op)
    }

    val requester = TestProbe()
    val topNode = system.actorOf(Props[BinaryTreeSet])
    val count = 1000

    val ops = randomOperations(requester.ref, count)
    val expectedReplies = referenceReplies(ops)

    ops foreach { op =>
      topNode ! op
      if (rnd.nextDouble() < 0.1) topNode ! GC
    }
    receiveN(requester, ops, expectedReplies)
  }

  // EMD
  test("Test empty tree 'Contains'") {
    val testNode = system.actorOf(BinaryTreeNode.props(0, true))

    testNode ! Contains(testActor, id = 1, 0)
    expectMsg(ContainsResult(1, false))
  }
  
  // EMD
  test("Test empty tree 'Insert'") {
    val testNode = system.actorOf(BinaryTreeNode.props(0, true))

    testNode ! Insert(testActor, id = 1, 42)
    expectMsg(OperationFinished(1))
    testNode ! Contains(testActor, id = 2, 42)
    expectMsg(ContainsResult(2, true))
  }

  test("GC does not break tree semantics") {
    val topNode = system.actorOf(Props[BinaryTreeSet])
 
    topNode ! Insert(testActor, id = 1, 5)
    expectMsg(OperationFinished(1))
 
    topNode ! Insert(testActor, id = 2, 4)
    expectMsg(OperationFinished(2))
 
    topNode ! Insert(testActor, id = 3, 6)
    expectMsg(OperationFinished(3))
 
    topNode ! GC
 
    topNode ! Contains(testActor, id = 4, 5)
    expectMsg(ContainsResult(4, true))
 
    topNode ! Contains(testActor, id = 5, 4)
    expectMsg(ContainsResult(5, true))
 
    topNode ! Contains(testActor, id = 6, 6)
    expectMsg(ContainsResult(6, true))
 
    topNode ! Contains(testActor, id = 7, 9)
    expectMsg(ContainsResult(7, false))
  }

  // EMD
 test("GC works well for predefined tree") {
    val topNode = system.actorOf(Props[BinaryTreeSet])
       
    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))
      
    topNode ! Insert(testActor, id = 2, 1)
    topNode ! Contains(testActor, id = 3, 1)
 
    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
       
    topNode ! Remove(testActor, id = 4, 1)
    expectMsg(OperationFinished(4))
       
    topNode ! Contains(testActor, id = 5, 1)
    expectMsg(ContainsResult(5, false))
       
    topNode ! Insert(testActor, id = 6, 10)
    expectMsg(OperationFinished(6))
      
    topNode ! Insert(testActor, id = 7, 11)
    topNode ! Remove(testActor, id = 8, 11)
    expectMsg(OperationFinished(7))
    expectMsg(OperationFinished(8))
      
    topNode ! GC
       
    topNode ! Contains(testActor, id = 9, 10)
    expectMsg(ContainsResult(9, true))      
   }

   // EMD
  test("Simple instruction With GC"){
    val topNode = system.actorOf(Props[BinaryTreeSet])
 
    topNode ! GC
    topNode ! Contains(testActor, id = 1, 1)
    expectMsg(ContainsResult(1, false))
 
    topNode ! GC
 
    topNode ! Insert(testActor, id = 2, 2)
    topNode ! Contains(testActor, id = 3, 2)
 
    expectMsg(OperationFinished(2))
    expectMsg(ContainsResult(3, true))
 
    topNode ! Remove(testActor, id = 4, 1)
 
    topNode ! GC
 
    topNode ! Contains(testActor, id = 5, 1)
 
    expectMsg(OperationFinished(4))
    expectMsg(ContainsResult(5, false))
  }

  // EMD
  test("inserting element that have beed removed") {
    val requester = TestProbe()
    val requesterRef = requester.ref
    val ops = List(
      Insert(requesterRef, id=10, 1),
      Contains(requesterRef, id=20, 1),
      Remove(requesterRef, id=30, 1),
      Contains(requesterRef, id=40, 1),
      Insert(requesterRef, id=50, 1),
      Contains(requesterRef, id=60, 1)
    )
 
    val expectedReplies = List(
      OperationFinished(id=10),
      ContainsResult(id=20, true),
      OperationFinished(id=30),
      ContainsResult(id=40, false),
      OperationFinished(id=50),
      ContainsResult(id=60, true)
    )
 
    verify(requester, ops, expectedReplies)
  }
}
