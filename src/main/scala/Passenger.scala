import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.StashBuffer
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.TimerScheduler
import scala.concurrent.duration._
import akka.actor.typed.ActorRef
import scala.util.Random

object Passenger {
    sealed trait Command
    private case object Timeout extends Command

    def apply(passengerID: Int, maxFloors: Int): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Passenger(context, passengerID, maxFloors).thinking 
        }
    }
}

class Passenger(context: ActorContext[Passenger.Command], passengerID: Int, maxFloors: Int) {

    val maxThinkTime: FiniteDuration = 10.seconds
    var decide_floor: Int = -1

    private def thinking: Behavior[Passenger.Command] = {
        context.log.info(s"[Passenger $passengerID]: thinking..")
        Behaviors.withTimers[Passenger.Command] { timers =>
            timers.startSingleTimer(Passenger.Timeout, think_time)
            Behaviors.receiveMessage {
                case Passenger.Timeout =>
                    decide_floor = Random.nextInt(maxFloors)
                    context.log.info(s"[Passenger $passengerID]: decided to go to floor ${decide_floor}")
                    waiting
            }
        }   
    }

    private def waiting: Behavior[Passenger.Command] = {
        Behaviors.same
    }

    private def think_time: FiniteDuration = {
        val r = new Random()
        r.nextInt(maxThinkTime.toSeconds.toInt).seconds
    }
}