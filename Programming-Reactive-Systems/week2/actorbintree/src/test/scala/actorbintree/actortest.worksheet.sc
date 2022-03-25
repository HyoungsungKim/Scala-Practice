import $dep.`com.typesafe.akka::akka-stream:2.6.19`
import $dep.`com.typesafe.akka::akka-testkit:2.6.19`

import akka.actor.*
import akka.testkit.{ImplicitSender, TestKit, TestProbe}

class Toggle extends Actor:
    def happy: Receive = 
        case "How are you?" =>
            sender() ! "happy"
            context.become(sad)

    def sad: Receive = 
        case "How are you?" =>
            sender() ! "sad"
            context.become(happy)

    def receive = happy
end Toggle

import scala.concurrent.duration._

given system:ActorSystem  = ActorSystem("TestSys")

object SimplesActorTest:
    val toggle = system.actorOf(Props[Toggle]())
    val p = TestProbe()

    p.send(toggle, "How are you")
    p.expectMsg("happy")
    p.send(toggle, "How are you")
    p.expectMsg("sad")
    p.send(toggle, "unknown")
    p.expectNoMessage(1.second)
    system.terminate()

    new TestKit(ActorSystem("TestSys")) with ImplicitSender:
        val toggle = this.system.actorOf(Props[Toggle]())
        toggle ! "How are you?"
        expectMsg("happy")
        toggle ! "How are you?"
        expectMsg("sad")
        toggle ! "unknown"
        expectNoMessage(1.second)
        this.system.terminate()
    
    println("Done")


/*
    Accessing the real DB or production web services is not desirable
    - one simple solution is to add overridable factory methods
*/