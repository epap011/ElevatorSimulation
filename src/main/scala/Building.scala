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
}

class Building(context: ActorContext[Building.BuildingMessage]) extends AbstractBehavior[Building.BuildingMessage](context) {
    override def onMessage(msg: Building.BuildingMessage): Behavior[Building.BuildingMessage] = {
        msg match {
            case Building.StartBuilding(elevatorsNum, floorsNum, passengersNum) => 
                val elevatorActors = (1 to numberOfElevators).map { elevatorId =>
                    context.spawn(Elevator(elevatorId), s"elevator-$elevatorId")
                }
                val floorActors = (1 to floorsNum).map { floorId =>
                    context.spawn(Floor(floorId), s"floor-$floorId")
                }
                val passengerActors = (1 to passengersNum).map { passengerId =>
                    context.spawn(Passenger(passengerId), s"passenger-$passengerId")
                }
                this
        }
    }

    override def onSignal: PartialFunction[Signal, Behavior[Building.BuildingMessage]] = {
        case PostStop =>
            context.log.info("Building stopped")
            this
    }
}