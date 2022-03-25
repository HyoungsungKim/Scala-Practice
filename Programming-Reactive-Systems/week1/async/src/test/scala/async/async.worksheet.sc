import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.util.control.NonFatal

// https://docs.scala-lang.org/overviews/scala-book/futures.html#inner-main
def aShortRunningTask(): Int = 42
val x = aShortRunningTask()

val a = Future { Thread.sleep(10 * 1000); 42}
val b = a.map(_ * 2)
b

def getStockPrice(stockSymbol: String): Future[Double] = Future {
    val r = scala.util.Random
    val randomSleepTime = r.nextInt(3000)
    val randomPrice = r.nextDouble * 1000
    //Thread.sleep(randomSleepTime)
    randomPrice
}

getStockPrice("aapl")

def getStockExist(stockSymbol: String): Future[Boolean] = Future {
    true
}

getStockExist("aapl")

val q = {
 println("q")
 1
}

def w = {
 println("w")
 2
}

q + q
q + w
w + w

val r = new PartialFunction[Int, Int] 
{

    // Applying isDefinedAt method 
    def isDefinedAt(q: Int) = q != 0

    // Applying apply method
    def apply(q: Int) = 12 * q

} 

// Displays output if the
// condition is satisfied

r(10)
r.lift(10)

val isOdd: PartialFunction[Int, String] = {
  case x if x % 2 == 1 => s"$x is Odd"
  case _ => "It is not Odd"
}

val isEven: PartialFunction[Int, String] = {
  case x if x % 2 == 0 => s"$x is Even"
}


isOdd(1)

List(1,2,3).map(isOdd(_))
List(1,2,3).map(isEven.lift(_))


import scala.util.{Success, Failure}

def sequenceComputations[A, B](
makeAsyncComputation1: () => Future[A],
makeAsyncComputation2: () => Future[B]
): Future[(A, B)] =
makeAsyncComputation1().flatMap{
    case Success(compu1: A) => makeAsyncComputation2().flatMap {
    case Success(compu2: B) => Future(compu1, compu2)
    case Failure(throwable) => throw throwable
    }
    case Failure(throwable) => throw throwable
}

// futurize(callbackBasedApi).computeIntAsync
  /**
    * Turns a callback-based API into a Future-based API
    * @return A `FutureBasedApi` that forwards calls to `computeIntAsync` to the `callbackBasedApi`
    *         and returns its result in a `Future` value
    *
    * Hint: Use a `Promise`
    */

def futurize(callbackBasedApi: CallbackBasedApi): FutureBasedApi =
  /*
  var integer: Int = _
  val promise = Promise[FutureBasedApi]()

  callbackBasedApi.computeIntAsync(integer => 
    promise.complete()
  )
  */
  var value: Int = 0
  val promiseInt = Promise[Int]
  val f = promiseInt.future

  val producer = Future {
    val r = callbackBasedApi.computeIntAsync{
      x => value = x.get
    }
    value    
  }
  
  //promise.future

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
