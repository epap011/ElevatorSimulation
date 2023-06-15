import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.StashBuffer
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.TimerScheduler
import scala.concurrent.duration._
import scala.collection.immutable.SortedSet
import akka.actor.typed.ActorRef

object Elevator {
    sealed trait Command
    private case object Timeout extends Command
    final case class CallElevator(floorID: Int, replyTo: ActorRef[Floor.FloorReached]) extends Command
    final case class FloorReached(floorID: Int)
    
    def apply(elevatorID: Int): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Elevator(context, elevatorID).start 
        }
    }
}

class Elevator(context: ActorContext[Elevator.Command], elevatorID: Int) {
    val timePerFloor: Int = 1
    var currentFloor: Int = 0

    var floorCallRequests = SortedSet[(Int, ActorRef[Floor.FloorReached])]()

    private def start: Behavior[Elevator.Command] = idle

    private def idle: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Idle")
        Behaviors.receiveMessage {
            case Elevator.CallElevator(floor, replyTo) =>
                context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                floorCallRequests += ((floor, replyTo))
                travelling
            
            case Elevator.Timeout => Behaviors.same
        }
    }

    private def waiting: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Waiting")
        Behaviors.withTimers[Elevator.Command] { timers =>
            timers.startSingleTimer(Elevator.Timeout, 10.seconds)
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    floorCallRequests += ((floor, replyTo))
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    if(floorCallRequests.isEmpty) idle
                    else travelling
            }
        }
    }

    private def travelling: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Travelling")
        Behaviors.withTimers[Elevator.Command] { timers =>
            val nextFloor = floorCallRequests.head
            val timeToNextFloor = Math.abs(nextFloor._1 - currentFloor) * timePerFloor
            timers.startSingleTimer(Elevator.Timeout, timeToNextFloor.seconds)
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    floorCallRequests += ((floor, replyTo))
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    currentFloor = nextFloor._1

                    floorCallRequests.find { case (floor, _) => floor == currentFloor } match {
                        case Some((_, targetActorRef)) =>
                            targetActorRef ! Floor.FloorReached(elevatorID)
                        case None =>
                            throw new NoSuchElementException(s"No actor reference found for floor $currentFloor")
                    }
                    floorCallRequests -= nextFloor
                    waiting
            }
        }
    }
}