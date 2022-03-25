package example

import akka.actor.Actor
import akka.event.LoggingReceive

object BankAccount:
    case class Deposit(amount: Bigint):
        require(amount > 0)
    case class Withdraw(amount: BigInt):
        require(amount > 0)
    case object Done
    case object Failed
    
class BankAccount extends Actor:
    import BankAccount._
    var balnce = BigInt(0)

    def receive = 
        case Deposit(amount) => {
            balance += amount
            sender ! Done   // Send ack
        }
        case Withdraw(amount) if amount <= balance => {
            balance -= amount
            sender ! Done
        }
        case _ => {
            sender ! Failed
        }

object WireTransfer:
    case class Transfer(from: ActorRef, to: ActorRef, amount: BigInt)        
    case object Done
    case object Failed

class WireTransfer extends Actor:
    import WireTransfer._

    def receive = 
        case Transfer(from, to, amount) => {
            from ! BankAccount.Withdraw(amount)
            context.become(awaitWithdraw(to, amount, sender))
        }

    def awaitWithdraw(to: ActorRef, amount: BigInt, sender: ActorRef): Receive = 
        case BankAccount.Done =>
            to ! BancAccount.Deposit(amount)
            context.become(awaitDeposit(client))
        case BankAccount.Failed =>
            client ! Failed
            context.stop(self)

    def awaitDeposit(client: ActorRef): Receive = 
        case BankAccount.Done => 
            client ! Done
            context.stop(self)