import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.Signal
import akka.actor.typed.PostStop
import akka.actor.typed.ActorRef

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
    var elevatorActors:Seq[ActorRef[Elevator.Command]] = Seq.empty
    var floorActors   :Seq[ActorRef[Floor.Command]] = Seq.empty
    var passengerActors:Seq[ActorRef[Passenger.Command]] = Seq.empty

    override def onMessage(msg: Building.BuildingMessage): Behavior[Building.BuildingMessage] = {
        msg match {
            case Building.StartBuilding(elevatorsNum, floorsNum, passengersNum) =>
                elevatorActors = (1 to elevatorsNum).map { elevatorId =>
                    context.spawn(Elevator(elevatorId), s"elevator-$elevatorId")
                }
                floorActors = (0 to floorsNum-1).map { floorId =>
                    context.spawn(Floor(floorId, elevatorActors), s"floor-$floorId")
                }
                val floorActorOpt = floorActors.find(_.path.name.endsWith(s"floor-0"))
                floorActorOpt match {
                    case Some(floorActor) =>
                        passengerActors = (1 to passengersNum).map { passengerId =>
                            context.spawn(Passenger(passengerId, floorsNum, floorActor), s"passenger-$passengerId")
                        }
                    case None =>
                        context.log.error("Unreachable: Floor 0 not found")
                }

                Behaviors.same
            
            case Building.ActivateElevators =>
                // context.log.info("Activating elevators..")
                // elevatorActors.foreach { elevatorActor =>
                //     elevatorActor ! Elevator.CallElevator(2, floorActors(2))
                //     elevatorActor ! Elevator.CallElevator(4, floorActors(4))
                // }
                Behaviors.same
        }
    }

    override def onSignal: PartialFunction[Signal, Behavior[Building.BuildingMessage]] = {
        case PostStop =>
            context.log.info("Building stopped")
            this
    }
}