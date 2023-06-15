import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.Signal
import akka.actor.typed.PostStop

object Building {
    def apply(): Behavior[BuildingMessage] =
        Behaviors.setup(context => new Building(context))
    
    sealed trait BuildingMessage
    final case class StartBuilding(elevatorsNum: Int, floorsNum: Int, passengersNum: Int) extends BuildingMessage
    case object ActivateElevators extends BuildingMessage
}

class Building(context: ActorContext[Building.BuildingMessage]) extends AbstractBehavior[Building.BuildingMessage](context) {
    val elevatorsNum  :Int = 0
    val floorsNum     :Int = 0
    val passengersNum :Int = 0
    var elevatorActors:Seq[akka.actor.typed.ActorRef[Elevator.Command]] = Seq.empty
    var floorActors   :Seq[akka.actor.typed.ActorRef[Floor.Command]] = Seq.empty
    // var passengerActors:Seq[akka.actor.typed.ActorRef[Passenger.PassengerMessage]] = Seq.empty

    override def onMessage(msg: Building.BuildingMessage): Behavior[Building.BuildingMessage] = {
        msg match {
            case Building.StartBuilding(elevatorsNum, floorsNum, passengersNum) => 
                floorActors = (1 to floorsNum).map { floorId =>
                    context.spawn(Floor(floorId), s"floor-$floorId")
                }
                elevatorActors = (1 to elevatorsNum).map { elevatorId =>
                    context.spawn(Elevator(elevatorId), s"elevator-$elevatorId")
                }
                // val passengerActors = (1 to passengersNum).map { passengerId =>
                //     context.spawn(Passenger(passengerId), s"passenger-$passengerId")
                // }
                Behaviors.same
            
            case Building.ActivateElevators =>
                context.log.info("Activating elevators..")
                elevatorActors.foreach { elevatorActor =>
                    elevatorActor ! Elevator.CallElevator(1, floorActors(0))
                    elevatorActor ! Elevator.CallElevator(5, floorActors(4))
                }
                Behaviors.same
        }
    }

    override def onSignal: PartialFunction[Signal, Behavior[Building.BuildingMessage]] = {
        case PostStop =>
            context.log.info("Building stopped")
            this
    }
}