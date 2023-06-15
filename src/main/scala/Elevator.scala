import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.StashBuffer
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.TimerScheduler
import scala.concurrent.duration._
import scala.collection.immutable.SortedSet

object Elevator {
    sealed trait Command
    private case object Timeout extends Command
    final case class CallElevator(floorID: Int) extends Command
    final case class FloorReached(floorID: Int)
    
    def apply(elevatorID: Int): Behavior[Command] = {
        Behaviors.withStash(100) { buffer =>
            Behaviors.setup[Command] { context =>
                new Elevator(context, buffer, elevatorID).start
            }
        }
    }
}

class Elevator(
    context: ActorContext[Elevator.Command],
    buffer: StashBuffer[Elevator.Command],
    elevatorID: Int) {
    
    val timePerFloor: Int = 1
    var currentFloor: Int = 0

    var floorCallRequests = SortedSet[Int]()

    private def start: Behavior[Elevator.Command] = idle

    private def idle: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Idle")
        Behaviors.receiveMessage {
            case Elevator.CallElevator(floor) =>
                context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                floorCallRequests += floor
                waiting
            
            case Elevator.Timeout => Behaviors.same
        }
    }

    private def waiting: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: Waiting")
        Behaviors.withTimers[Elevator.Command] { timers =>
            timers.startSingleTimer(Elevator.Timeout, 10.seconds)
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    floorCallRequests += floor
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
            floorCallRequests -= nextFloor
            val timeToNextFloor = (nextFloor - currentFloor).abs * timePerFloor
            timers.startSingleTimer(Elevator.Timeout, timeToNextFloor.seconds)
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    floorCallRequests += floor
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    context.log.info(s"[Elevator $elevatorID]: reaches floor $nextFloor!")
                    currentFloor = nextFloor
                    waiting
            }
        }
    }
}