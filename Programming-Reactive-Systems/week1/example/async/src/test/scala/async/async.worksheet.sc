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