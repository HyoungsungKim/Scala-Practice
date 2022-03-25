package example2

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, Terminated }
import akka.{ NotUsed }

import java.net.{URLEncoder}
import java.nio.charset.StandardCharsets

object ChatRoom:
    sealed trait RoomCommand
    final case class GetSession(screenName: String, replyTo: ActorRef[SessionEvent]) extends RoomCommand
    
    sealed trait SessionEvent
    final case class SessionGranted(handle: ActorRef[PostMessage]) extends SessionEvent
    final case class SessionDenied(reason: String) extends SessionEvent
    final case class MessagePosted(screenName: String, message: String) extends SessionEvent

    sealed trait SessionCommand
    final case class PostMessage(message: String) extends SessionCommand

    private final case class NotifyClient(message: MessagePosted) extends SessionCommand
    private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

    def apply(): Behavior[RoomCommand] = 
        chatRoom(List.empty)

    private def chatRoom(sessions: List[ActorRef[SessionCommand]]): Behavior[RoomCommand] = 
        Behaviors.receive { (context, message) => message match
            case GetSession(screenName, client) =>
                val ses = context.spawn(
                    session(context.self, screenName, client),
                    name = URLEncoder.encode(screenName, StandardCharsets.UTF_8.name)
                )
                client ! SessionGranted(ses)
                chatRoom(ses :: sessions)
            case PublishSessionMessage(screenName, message) =>
                val notification = NotifyClient(MessagePosted(screenName, message))
                sessions.foreach(_ ! notification)
                Behaviors.same
        }

    private def session(
        room: ActorRef[PublishSessionMessage],
        screenName: String,
        client: ActorRef[SessionEvent]
    ): Behavior[SessionCommand] = 
        Behaviors.receiveMessage {
            case PostMessage(message) =>
                room ! PublishSessionMessage(screenName, message)
                Behaviors.same
            case NotifyClient(message) =>
                client ! message
                Behaviors.same
        }

end ChatRoom

object Gabbler:
    import ChatRoom._

    def apply(): Behavior[SessionEvent] = 
        Behaviors.setup { context =>
            Behaviors.receiveMessage {
                case SessionGranted(handle) =>
                    handle ! PostMessage("Hello World!")
                    Behaviors.same
                case MessagePosted(screenName, message) =>
                    context.log.info2("Message has been posted by '{}': {}", screenName, message)
                    Behaviors.stopped
            }
        }
end Gabbler

object Main extends App:
    def apply(): Behavior[NotUsed] = 
        Behaviors.setup { context =>
            val chatRoom = context.spawn(ChatRoom(), "chatroom")
            val gabblerRef = context.spawn(Gabbler(), "gabbler")
            context.watch(gabblerRef)
            chatRoom ! ChatRoom.GetSession("ol' Gabbler", gabblerRef)

            Behaviors.receiveSignal {
                case (_, Terminated(_)) =>
                    Behaviors.stopped
            }
        }

    ActorSystem(Main(), "ChatRoomDemo")
end Main        