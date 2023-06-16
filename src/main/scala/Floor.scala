import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.ActorRef

object Floor {
    sealed trait Command
    final case class FloorReached(elevatorID: Int, replyTo: ActorRef[Elevator.Command]) extends Command
    final case class CallElevator(floorID: Int, replyTo: ActorRef[Passenger.Command])   extends Command
    final case class Departure(elevatorID: Int)                                         extends Command

    def apply(floorID: Int, elevatorActors: Seq[ActorRef[Elevator.Command]]): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Floor(context, floorID, elevatorActors).start
        }
    }
}

class Floor(
    context: ActorContext[Floor.Command], 
    floorID: Int,
    elevatorActors: Seq[ActorRef[Elevator.Command]]) {

    var arrivedElevators:  Seq[ActorRef[Elevator.PassengerEntered]] = Seq()
    var waitingPassengers: Seq[(Int, ActorRef[Passenger.Command])]  = Seq()

    private def start: Behavior[Floor.Command] = {
        Behaviors.receiveMessage {
            case Floor.FloorReached(elevatorID, replyTo) =>
                context.log.info(s"[Floor $floorID]: Elevator $elevatorID reached")
                arrivedElevators = arrivedElevators :+ replyTo
                for(passenger <- waitingPassengers) {
                    replyTo ! Elevator.PassengerEntered(passenger._1, passenger._2)
                }
                waitingPassengers = Seq()
                Behaviors.same
            
            case Floor.Departure(elevatorID) =>
                context.log.info(s"[Floor $floorID]: Elevator $elevatorID departed")
                //arrivedElevators = arrivedElevators.filterNot(_ == context.sender)
                Behaviors.same
            
            case Floor.CallElevator(floorIDToGo, replyTo) =>
                context.log.info(s"[Floor $floorID]: passenger called elevator to go to floor $floorIDToGo")
                waitingPassengers = waitingPassengers :+ ((floorIDToGo, replyTo))
                if(arrivedElevators.nonEmpty) {
                    arrivedElevators.head ! Elevator.PassengerEntered(floorIDToGo, replyTo)
                    waitingPassengers = waitingPassengers.tail
                }
                else {
                    for(elevator <- elevatorActors) {
                        elevator ! Elevator.CallElevator(floorID, context.self)
                    }
                }
                Behaviors.same
        }
    }
}