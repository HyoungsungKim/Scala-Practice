package async

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.util.control.NonFatal
import javax.xml.ws.FaultAction

object Async extends AsyncInterface:

  /**
    * Transforms a successful asynchronous `Int` computation
    * into a `Boolean` indicating whether the number was even or not.
    * In case the given `Future` value failed, this method
    * should return a failed `Future` with the same error.
    */
  def transformSuccess(eventuallyX: Future[Int]): Future[Boolean] =
    eventuallyX.flatMap(
      elem => Future{ elem % 2 == 0}
    )
  /**
    * Transforms a failed asynchronous `Int` computation into a
    * successful one returning `-1`.
    * Any non-fatal failure should be recovered.
    * In case the given `Future` value was successful, this method
    * should return a successful `Future` with the same value.
    */
  def recoverFailure(eventuallyX: Future[Int]): Future[Int] =
    def pf:PartialFunction[Throwable, Int] = { 
      case e: Throwable => -1
    }
    
    eventuallyX.recover(pf)
    /*
    eventuallyX.recover(err match
      case e: Throwable => Future{ -1 }
    )
    */

  /**
    * Perform two asynchronous computation, one after the other. `makeAsyncComputation2`
    * should start ''after'' the `Future` returned by `makeAsyncComputation1` has
    * completed.
    * In case the first asynchronous computation failed, the second one should not even
    * be started.
    * The returned `Future` value should contain the successful result of the first and
    * second asynchronous computations, paired together.
    */
  def sequenceComputations[A, B](
    makeAsyncComputation1: () => Future[A],
    makeAsyncComputation2: () => Future[B]
  ): Future[(A, B)] =
    import scala.util.{Success, Failure}
    
    /*
    makeAsyncComputation1().flatMap{
      case Success(compu1: A) => makeAsyncComputation2().map {
        case Success(compu2: B) => (compu1, compu2)
        case Failure(throwable) => throw throwable
      }
      case Failure(throwable) => throw throwable
    }
    */
    makeAsyncComputation1().flatMap( res => makeAsyncComputation2().map(res2 => (res, res2)))

  /**
    * Concurrently perform two asynchronous computations and pair their successful
    * result together.
    * The two computations should be started independently of each other.
    * If one of them fails, this method should return the failure.
    */
  def concurrentComputations[A, B](
    makeAsyncComputation1: () => Future[A],
    makeAsyncComputation2: () => Future[B]
  ): Future[(A, B)] =
    import scala.util.{Success, Failure}
    
    val ziped = makeAsyncComputation1().zip(makeAsyncComputation2())
    ziped.flatMap{
      case (_, Failure(throwable)) => throw throwable
      case (Failure(throwable), _) => throw throwable
      case (Success(compu1: A), Success(compu2: B)) => Future(compu1, compu2)
    }

  /**
    * Attempt to perform an asynchronous computation.
    * In case of failure this method should try again to make
    * the asynchronous computation so that at most `maxAttempts`
    * are eventually performed.
    */
  def insist[A](makeAsyncComputation: () => Future[A], maxAttempts: Int): Future[A] =
    import scala.util.{Success, Failure}

    /*
    def tryAgain(callBack: () => Future[A], attempts: Int)(maxAttempts: Int): Future[A] = 
      callBack().flatMap {
        case Success(res: A) => Future(res)
        case Failure(throwable) if attempts <= maxAttempts => tryAgain(callBack, attempts+1)(maxAttempts)
        case Failure(throwable) if attempts > maxAttempts => throw throwable
      }
     */

    def tryAgain(callBack: () => Future[A], attempts: Int)(maxAttempts: Int): Future[A] = 
      if attempts == maxAttempts then Future.failed(new RuntimeException("Exceed max attempts"))
      else
        callBack().recoverWith {
          case e: Throwable => tryAgain(callBack, attempts+1)(maxAttempts)
        }
   
    tryAgain(makeAsyncComputation, 0)(maxAttempts)


  /**
    * Turns a callback-based API into a Future-based API
    * @return A `FutureBasedApi` that forwards calls to `computeIntAsync` to the `callbackBasedApi`
    *         and returns its result in a `Future` value
    *
    * Hint: Use a `Promise`
    */
  def futurize(callbackBasedApi: CallbackBasedApi): FutureBasedApi = 
    var value: Int = 0
    //val promiseInt = Promise[Int]
    //val f = promiseInt.future

    val producer = Future {
      val r = callbackBasedApi.computeIntAsync{
        x => value = x.get
      }
      value    
    }

    () => producer

/**
  * Dummy example of a callback-based API
  */
trait CallbackBasedApi:
  def computeIntAsync(continuation: Try[Int] => Unit): Unit

/**
  * API similar to [[CallbackBasedApi]], but based on `Future` instead
  */
trait FutureBasedApi:
  def computeIntAsync(): Future[Int]
