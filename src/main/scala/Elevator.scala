import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.Signal
import akka.actor.typed.PostStop

object Elevator {
    def apply(elevatorID: Int): Behavior[ElevatorMessage] =
        Behaviors.setup(context => new Elevator(context, elevatorID))
    
    sealed trait ElevatorMessage
    final case class FloorButtonPressed(floorID: Int)    extends ElevatorMessage
    final case class ElevatorButtonPressed(floorID: Int) extends ElevatorMessage
}

class Elevator(context: ActorContext[Elevator.ElevatorMessage], elevatorID: Int) extends AbstractBehavior[Elevator.ElevatorMessage](context) {

    var currentFloor: Int = 0
    val timePerFloor: Int = 1000*5

    override def onMessage(msg: Elevator.ElevatorMessage): Behavior[Elevator.ElevatorMessage] = {
        msg match {
            case Elevator.FloorButtonPressed(floorID: Int) =>
                context.log.info(s"Elevator $elevatorID received request to go to floor $floorID")
                val result = moveElevator(floorID)
                context.log.info(s"Elevator $elevatorID reaches floor $floorID")
                Behaviors.same
            
            case Elevator.ElevatorButtonPressed(floorID: Int)  => 
                context.log.info(s"Elevator $elevatorID received request to go to floor $floorID")
                val result = moveElevator(floorID)
                context.log.info(s"Elevator $elevatorID reaches floor $floorID")
                Behaviors.same
        }
    }

    override def onSignal: PartialFunction[akka.actor.typed.Signal, Behavior[Elevator.ElevatorMessage]] = {
        case akka.actor.typed.PostStop =>
            context.log.info(s"Elevator $elevatorID stopped")
            this
    }

    private def moveElevator(floorNumber: Int): String = {
        if (currentFloor < floorNumber) {
            while (currentFloor < floorNumber) {
                currentFloor += 1
                Thread.sleep(timePerFloor)
            }
        } else 
        if (currentFloor > floorNumber) {
            while (currentFloor > floorNumber) {
                currentFloor -= 1
                Thread.sleep(timePerFloor)
            }
        }
        "Arrived"
    }
}