import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps

object Floor {
    def apply(floorId: Int): Behavior[FloorMessage] =
        Behaviors.setup(context => new Floor(context, floorId))
    
    sealed trait FloorMessage
    final case class ElevatorArrived(elevatorID: Int)  extends FloorMessage
    final case class ElevatorDeparted(elevatorID: Int) extends FloorMessage
}

class Floor(context: ActorContext[Floor.FloorMessage], floorId: Int) extends AbstractBehavior[Floor.FloorMessage](context) {
    override def onMessage(msg: Floor.FloorMessage): Behavior[Floor.FloorMessage] = {
        msg match {
            case Floor.ElevatorArrived(elevatorID: Int) =>
                context.log.info(s"Elevator $elevatorID arrived at Floor $floorId")
                Behaviors.same
            
            case Floor.ElevatorDeparted(elevatorID: Int)  => 
                context.log.info(s"Elevator $elevatorID departed from Floor $floorId")
                Behaviors.stopped
        }
    }

    override def onSignal: PartialFunction[akka.actor.typed.Signal, Behavior[Floor.FloorMessage]] = {
        case akka.actor.typed.PostStop =>
            context.log.info(s"Floor $floorId stopped")
            this
    }
}