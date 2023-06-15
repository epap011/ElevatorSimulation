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
    final case class CallElevator(floorID: Int, replyTo: ActorRef[Floor.Command]) extends Command
    final case class FloorReached(elevatorID: Int, replyTo: ActorRef[Elevator.Command]) extends Command
    final case class Departure(elevatorID: Int) extends Command
    final case class PassengerEntered(passenger: ActorRef[Passenger.Command]) extends Command
    
    def apply(elevatorID: Int): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Elevator(context, elevatorID).start 
        }
    }
}

class Elevator(context: ActorContext[Elevator.Command], elevatorID: Int) {
    val timePerFloor: Int = 1
    var currentFloor: Int = 0

    var floorCallRequests = SortedSet[(Int, ActorRef[Floor.Command])]()
    var passengers: Seq[ActorRef[Passenger.Command]] = Seq()

    private def start: Behavior[Elevator.Command] = idle

    private def idle: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Idle at floor $currentFloor")
        Behaviors.receiveMessage {
            case Elevator.CallElevator(floor, replyTo) =>
                context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                floorCallRequests += ((floor, replyTo))
                travelling
            
            case Elevator.Timeout => Behaviors.same
        }
    }

    private def waiting: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Waiting at floor $currentFloor")
        Behaviors.withTimers[Elevator.Command] { timers =>
            timers.startSingleTimer(Elevator.Timeout, 10.seconds)
            Behaviors.receiveMessage {
                case Elevator.PassengerEntered(passenger) =>
                    context.log.info(s"[Elevator $elevatorID]: load passenger")
                    passengers :+= passenger
                    Behaviors.same

                case Elevator.CallElevator(floor, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    floorCallRequests += ((floor, replyTo))
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    if(floorCallRequests.isEmpty) idle
                    else {
                        floorCallRequests.find { case (floor, _) => floor == currentFloor } match {
                        case Some((_, targetActorRef)) =>
                            targetActorRef ! Floor.Departure(elevatorID)
                        case None =>
                            throw new NoSuchElementException(s"No actor reference found for floor $currentFloor")
                        }
                        travelling
                    }
            }
        }
    }

    private def travelling: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Travelling to floor ${floorCallRequests.head._1}")
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
                            targetActorRef ! Floor.FloorReached(elevatorID, context.self)
                        case None =>
                            throw new NoSuchElementException(s"No actor reference found for floor $currentFloor")
                    }
                    floorCallRequests -= nextFloor
                    waiting
            }
        }
    }
}