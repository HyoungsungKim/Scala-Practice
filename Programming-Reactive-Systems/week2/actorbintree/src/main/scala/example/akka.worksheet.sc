// Getter actor for processing the body
// Controller which spawns Getters for all links encountered
// Receptionist managing one Controller per request

import java.util.concurrent.Executor

import $dep.`com.typesafe.akka::akka-stream:2.6.19`
import akka.actor.*

import $dep.`com.ning:async-http-client:1.7.19`
import com.ning.http.client.*

case class BadStatus(statusCode: Int) extends RuntimeException

/*
    It blocks the calling actor until the web server has replied
    Actor is deaf to other request, e.g., cancellation does not work
    wastes one thread -> a finite resource

val client = new AsyncHttpClient
def get(url: String): String = 
    val response = client.prepareGet(url).execute().get // -> Block until website response
    if (response.getStatusCode < 400)
        response.getResponseBodyExcerpt(131072)
    else throw BadStatus(response.getStatusCode)
*/


/*
    A reactive application is non-blocking & event-driven top to bottom (비동기를 기본 방식으로, 동기 방식을 선택으로)

*/

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object WebClient:
    private val client = new AsyncHttpClient
    def get(url: String)(using exec: Executor): Future[String] =
        val f = client.prepareGet(url).execute()
        val p = Promise[String]()
        f.addListener(new Runnable {
            def run() = {
                val response = f.get
                if (response.getStatusCode < 400)
                    p.success(response.getResponseBodyExcerpt(131072))
                else p.failure(BadStatus(response.getStatusCode))
            }
        }, exec)
        p.future



import $dep.`org.jsoup:jsoup:1.14.3`
import org.jsoup.Jsoup
import scala.collection.JavaConverters._
import scala.util.{Success, Failure}

val url = "google.com"
def findLinks(body: String): Iterator[String] = 
    val document = Jsoup.parse(body, url)
    val links = document.select("a[href]")
    for {
        link <- links.iterator().asScala
    } yield link.absUrl("href")


import akka.pattern.*
import akka.{ Done }
// The getter actor
// Actors are run by a dispatcher -potentially shared- which can also run Futures
class Getter(url: String, depth: Int) extends Actor:
    given exec: Executor = context.dispatcher

    val future = WebClient.get(url)
    future.pipeTo(self)
    future.onComplete {
        case Success(body) => self ! body
        case Failure(err) => self ! Status.Failure(err)
    }

    def receive =
        case body: String =>
            for (link <- findLinks(body))
                context.parent ! Controller.Check(link, depth)
            stop()
        case _: Status.Failure => stop()
        case Abort  => stop()

    def stop(): Unit = 
        context.parent ! Done
        context.stop(self)

/*
    - Logging includes IO which can block idenfinitely
    - Akka's logging passes that task to dedicated actors
    - supports ActorSystem-wide levels of debug, info, warning, error
    - set level using setting akka.loglevel=DEBUG (for example)

*/
class A extends Actor with ActorLogging:
    def receive = 
        case msg => log.debug("received message: {}", msg)


import scala.concurrent.duration._
    
// The Controller
// - Prefer immutable data structures, since they can be shared
class Controller extends Actor with ActorLogging:
    context.setReceiveTimeout(10.seconds)
    var cache = Set.empty[String]
    var children = Set.empty[ActorRef]
    context.system.scheduler.scheduleOnce(10.second, self, Timeout){
        children.foreach(_ ! Getter.Abort)
    }

    def recevie = 
        case Check(url: String, depth: Int) =>
            log.debug("{} checking {}", depth, url)
            if (!cache(url) && depth > 0) then
                children += context.actorOf(Props(new Getter(url, depth - 1)))
            cache += url
        case Getter.Done =>
            children -= sender
            if (children.isEmpty) context.parent ! Result(cache)
        case Timeout => children.foreach(_ ! Getter.Abort)
end Controller

// - Do not refer to actor state from code running asynchronously
class Cache extends Actor:
    given exec: Executor = context.dispatcher
    var cache = Map.empty[String, String]            
    def receive = 
        case Get(url: String) =>
            if (cache.contains(url)) then sender ! cache(url)
            else
                val client = sender
                WebClient.get(url).map(Result(client, url, _)).pipeTo(self)
        case Result(client, url, body) =>
            cache += url -> body
            client ! body

class Receptionist extends Actor:
    def receive = waiting
    val waiting: Receive =
        case Get(url) => context.become(runNext(Vector(Job(sender, url))))

    case class Job(client: ActorRef, url: String)
    var reqNo = 0
    def runnext(queue: Vector[Job]): Receive = 
        reqNo += 1
        if (queue.isEmpty) then waiting
        else
            val controller = context.actorOf(Props[Controller], s"c$reqNo")
            controller ! Controller.Check(queue.head.url, 2)
            running(queue)


    def enqueuJob(queue: Vector[Job], job: Job): Receive = 
        if (queue.size > 3) then
            sender ! Failed(job.url)
            running(queue)
        else running(queue :+ job)



    def running(queue: Vector[Job]): Receive = 
        case Controller.Result(links) =>
            val job = queue.head
            job.client ! Result(job.url, links)
            context.stop(sender)
            context.become(runNext(queue.tail))
        case Get(url) =>
            context.become(enqueueJob(queue, Job(sender, url)))