import $dep.`com.typesafe.akka::akka-stream:2.6.19`
import $dep.`com.typesafe.akka::akka-actor-typed:2.6.19`
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

object HelloWorld: 
    final case class Greet(whom: String, replyTo: ActorRef[Greeted])
    final case class Greeted(whom: String, from: ActorRef[Greet])

    def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>
        context.log.info("Hello {}", message.whom)
        message.replyTo ! Greeted(message.whom, context.self)
        Behaviors.same
    }
end HelloWorld

object HelloWorldBot:
    def apply(max: Int): Behavior[HelloWorld.Greeted] = 
        bot(0, max)

    private def bot(greetingCounter: Int, max: Int): Behavior[HelloWorld.Greeted] = 
        Behaviors.receive{ (context, message) => 
            val n = greetingCounter + 1
            context.log.info2("Greeting {} for {}", n, message.from)    
            if (n == max) then
                Behaviors.stopped
            else
                message.from ! HelloWorld.Greet(message.whom, context.self)
                bot(n, max)
        }

end HelloWorldBot

object HelloWorldMain:
    final case class SayHello(name: String)

    def apply(): Behavior[SayHello] = 
        Behaviors.setup { context =>
            val greeter = context.spawn(HelloWorld(), "greeter")

            Behaviors.receiveMessage { message =>
                val replyTo = context.spawn(HelloWorldBot(max=3), message.name)
                greeter ! HelloWorld.Greet(message.name, replyTo)
                Behaviors.same
            }
        }

    def main(args: Array[String]): Unit = 
        println("Run Main")
        val system: ActorSystem[HelloWorldMain.SayHello] = 
            ActorSystem(HelloWorldMain(), "hello")

        println(system ! HelloWorldMain.SayHello("World"))
        println(system ! HelloWorldMain.SayHello("Akka"))

end HelloWorldMain        

val system: ActorSystem[HelloWorldMain.SayHello] = 
    ActorSystem(HelloWorldMain(), "hello")

system ! HelloWorldMain.SayHello("World")
system ! HelloWorldMain.SayHello("Akka")
println(system ! HelloWorldMain.SayHello("scala"))

HelloWorldMain.main(Array.empty)