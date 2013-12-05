package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.{Subject, Observable, Scheduler}
import rx.lang.scala.subjects.ReplaySubject
import rx.lang.scala.subscriptions.Subscription

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  // EMD
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    Observable { observer =>
      f onComplete {
        case Success(value) => observer.onNext(value); observer.onCompleted()
        case Failure(error) => observer.onError(error)
      }
      Subscription()
    }
  }
  // method 1
  // {
  //   val subject = ReplaySubject[T]()
  //   f.onComplete {
  //     case Success(x) => subject.onNext(x); subject.onCompleted()
  //     case Failure(e) => subject.onError(e)
  //   }

  //   subject
  // }

}
