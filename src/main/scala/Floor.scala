import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.Signal
import akka.actor.typed.PostStop

object Floor {
    sealed trait Command
    final case class FloorReached(elevatorID: Int)       extends Command

    def apply(floorID: Int): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Floor(context, floorID).start
        }
    }
}

class Floor(context: ActorContext[Floor.Command], floorID: Int) {

    private def start: Behavior[Floor.Command] = {
        Behaviors.receiveMessage {
            case Floor.FloorReached(elevatorID) =>
                context.log.info(s"[Floor $floorID]: Elevator $elevatorID reached")
                Behaviors.same
        }
    }
}