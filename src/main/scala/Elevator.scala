import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.StashBuffer
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.TimerScheduler
import scala.concurrent.duration._

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
    
    val timePerFloor: Int = 1000*5
    var currentFloor: Int = 0

    private def start: Behavior[Elevator.Command] = idle

    private def idle: Behavior[Elevator.Command] = {
        Behaviors.withTimers[Elevator.Command] { timers =>
            timers.startSingleTimer(Elevator.Timeout, 10.seconds)
            context.log.info(s"[Elevator $elevatorID]: Doors opened!")
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor) =>
                    context.log.info(s"[Elevator $elevatorID]: received CallElevator to floor $floor")
                    Behaviors.same
                
                case Elevator.Timeout =>
                    context.log.info(s"[Elevator $elevatorID]: Doors closed!")
                    timers.cancel(Elevator.Timeout)
                    Behaviors.same
            }
        }
    }

    private def travelToFloor(floorNumber: Int): Unit = {
        if (currentFloor < floorNumber) {
            while (currentFloor < floorNumber) {
                Thread.sleep(timePerFloor)
                currentFloor += 1
            }
        } 
        else 
        if (currentFloor > floorNumber) {
            while (currentFloor > floorNumber) {
                currentFloor -= 1
                Thread.sleep(timePerFloor)
            }
        }

    }
}