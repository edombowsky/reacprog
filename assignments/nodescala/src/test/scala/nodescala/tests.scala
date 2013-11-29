package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  // EMD
  def testExceptionOccurred[T](f: Future[T]): Boolean =
    try {
      Await.result(f, 1 second)
      false
    } catch {
      case TestException => true
  }

  // EMD
  case object TestException extends Exception

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  // EMD
  test("all success when all future succeed") {
    val all = Future.all[Int](List(Future.always(1), Future.always(2), Future.always(3)))

    assert(Await.result(all, 1 second) == List(1,2,3))
  }

  // EMD
  test("all returns a failure that returns when all future succeed") {
    val all = Future.all[Int](List(Future.always(1), Future.always(2), Future.failed(TestException)))

    assert(testExceptionOccurred(all))
  }

  // EMD
  test("delay timeout before delay") {
    val delay = Future.delay(100 milliseconds)
    try {
      Await.result(delay, 50 milliseconds)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  // EMD
  test("delay resolves after delay") {
    val delay = Future.delay(20 milliseconds)
    assert(Await.result(delay, 25 milliseconds).isInstanceOf[Unit])
  }

  // EMD
  test("any success when any future succeed") {
    val any = Future.any[Int](List(Future.always(1), Future.delay(2 seconds).map(u => 4)))

    assert(Await.result(any, 1 second) == 1, Await.result(any, 0 nanosecond))
  }

  // EMD
  test("any fails when any future failed") {
    val any = Future.any[Int](List(Future.failed(TestException), Future.delay(2 seconds).map(u => 4)))

    assert(testExceptionOccurred(any))
  }

  // EMD
  test("Future.all completes if all of the futures in the list have been completed") {
    val f1 = Future {1}
    val f2 = Future {2}
    val all = Future.all(List(f1, f2))

    assert(Await.result(all, 1 second) == List(1,2))
  }

  // EMD
  test("Future.all fails if any of the futures in the list cannot be completed") {
    val f1 = Future {1}
    val f2 = Future.never[Int]
    val all = Future.all(List(f1, f2))

    try {
      Await.result(all, 1 second)
      assert(false)
    } catch {
       case t: TimeoutException => // ok!
    }
  }

  // EMD
  test("Future.delay") {
    val delay = Future.delay(1 second)
    try {
      Await.result(delay, 2 second)
      assert(true)
    } catch {
      case t: TimeoutException => assert(false)
    }
  }

  // EMD
  test("Future.delay timeout") {
    val delay = Future.delay(2 second)
    try {
      Await.result(delay, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  // EMD
  test("A Future should not complete after 1s when using a delay of 3s") {

    val future = Future.delay(3 seconds)
    intercept[TimeoutException] {
      val future = Future.delay(3 seconds)
      Await.result(future, 1 second)
    }
  }

  // EMD
  test("Future.now") {
    val f = future {1}
    assert(f.now == 1)
  }

  // EMD
 test("Future.any") {
    val x = Promise[Int]();
    val f1 = future { x.tryComplete(Success(1)); 1}
    val f2 = future { x.tryComplete(Success(2)); 2}
    val f3 = future { x.tryComplete(Success(3)); 3}
    val any = Future.any(List(f1, f2, f3))
    val result = Await.result(any, 3 seconds)
    val xresult = Await.result(x.future, 3 seconds)
    assert(result == xresult)
  }

  // EMD
  test("continueWith should handle exceptions") {
    val future = Future.always(1)
    val continuation = future.continueWith(ftr => throw new RuntimeException("UH OH!"))
    Thread.sleep(10)
    assert(continuation.isCompleted)
    assert(continuation.failed.isCompleted)
  }

  // EMD
  test("continue should handle exceptions") {
    val future = Future.always(1)
    val continuation = future continue { case _ => throw new RuntimeException("DARN!") }
    Thread.sleep(10)
    assert(continuation.isCompleted)
    assert(continuation.failed.isCompleted)
  }

  // EMD
  test("CancellationTokenSource should remain cancelled after unsubscribed multiple times") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    cts.unsubscribe()
    cts.unsubscribe()
    assert(ct.isCancelled)
  }

  // EMD
  test("Future run can be stopped") {
    var state: Int = 0
    val work = Future.run() { token =>
      Future {
        while(token.nonCancelled) {
          state = 1
        }
        state = 2
      }
    }
    val working  = Future.delay(50 milliseconds).map { u =>
      state == 1
    }
    Future.delay(75 milliseconds) onSuccess {
      case _ => work.unsubscribe()
    }
    val stopped = Future.delay(100 milliseconds) map { u =>
      state == 2
    }
    assert(Await.result(working, 1 second))
    assert(Await.result(stopped, 1 second))
  }

  // EMD
  test("A Future should be able to delay") {
    import java.util.Date
    val start = new Date()
    println("Starting")
    val futureDelay = Future.delay(2 seconds)
    val now = new Date()
    println("Awaiting ...")
    Await.result(futureDelay, 2 seconds)
    println("End")
    val end = new Date()
    assert ((now.getTime - start.getTime) < 500)
    assert ((end.getTime - start.getTime) >= 2000)
  }

  // EMD
  test("FutureCompanionOps.run test") {
    val p = Promise[String]()
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          println("working")
        }
        println("done")
        p.success("done")
      }
    }
    Future.delay(.5 seconds) onSuccess {
      case _ => working.unsubscribe()
    }
    assert(Await.result(p.future, 1 second) == "done")
  }
}
