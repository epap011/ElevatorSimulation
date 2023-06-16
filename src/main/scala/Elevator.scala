import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.StashBuffer
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.TimerScheduler
import scala.concurrent.duration._
import scala.collection.immutable.SortedSet
import akka.actor.typed.ActorRef

object Elevator {
    sealed trait Command
    private case object Timeout extends Command
    final case class CallElevator(floorID: Int, replyTo: ActorRef[Floor.Command]) extends Command
    final case class FloorReached(elevatorID: Int, replyTo: ActorRef[Elevator.Command]) extends Command
    final case class Departure(elevatorID: Int) extends Command
    final case class PassengerEntered(floorToGo: Int, passenger: ActorRef[Passenger.Command]) extends Command
    final case class SetFloors(floors: Seq[ActorRef[Floor.Command]]) extends Command
    
    def apply(elevatorID: Int): Behavior[Command] = {
        Behaviors.setup[Command] { context =>
            new Elevator(context, elevatorID).start 
        }
    }
}

class Elevator(context: ActorContext[Elevator.Command], elevatorID: Int) {
    val timePerFloor: Int = 1
    var currentFloor: (Int, ActorRef[Floor.Command]) = null

    var floorCallRequests     = SortedSet[(Int, ActorRef[Floor.Command])]()
    var passengerCallRequests = SortedSet[(Int, ActorRef[Passenger.Command])]()
    var passengerList         = Seq[(Int, ActorRef[Passenger.Command])]()
    var floors: Seq[ActorRef[Floor.Command]] = Seq()

    private def start: Behavior[Elevator.Command] = setFloors

    private def idle: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: <Idle>")
        Behaviors.receiveMessage {
            case Elevator.CallElevator(floor, replyTo) =>
                context.log.info(s"[Elevator $elevatorID]: floor $floor called me")
                floorCallRequests += ((floor, replyTo))
                travelling
            
            case Elevator.Timeout => Behaviors.same
        }
    }

    private def waiting: Behavior[Elevator.Command] = {
        context.log.info(s"[Elevator $elevatorID]: <Waiting> at floor ${currentFloor._1}")
        Behaviors.withTimers[Elevator.Command] { timers =>
            timers.startSingleTimer(Elevator.Timeout, 10.seconds)
            Behaviors.receiveMessage {
                case Elevator.PassengerEntered(floorToGo, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: received a passenger with desination floor $floorToGo")
                    passengerCallRequests += ((floorToGo, replyTo))
                    passengerList = passengerList :+ ((floorToGo, replyTo))
                    Behaviors.same

                case Elevator.CallElevator(floor, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: floor $floor called me")
                    floorCallRequests += ((floor, replyTo))
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    if(floorCallRequests.isEmpty && passengerCallRequests.isEmpty) idle
                    else  travelling
            }
        }
    }

    private def travelling: Behavior[Elevator.Command] = {
        Behaviors.withTimers[Elevator.Command] { timers =>
            val nextFloor = nextFloorByPolicy
            context.log.info(s"[Elevator $elevatorID]: <Travelling> from floor ${ currentFloor._1} to $nextFloor")
            var timeToNextFloor: Int = 0
            timeToNextFloor = Math.abs(nextFloor - currentFloor._1) * timePerFloor
            timers.startSingleTimer(Elevator.Timeout, timeToNextFloor.seconds)
            currentFloor = (nextFloor,floors(nextFloor))
            
            Behaviors.receiveMessage {
                case Elevator.CallElevator(floor, replyTo) =>
                    context.log.info(s"[Elevator $elevatorID]: floor $floor called me")
                    floorCallRequests += ((floor, replyTo))
                    Behaviors.same
                
                case Elevator.Timeout =>
                    timers.cancel(Elevator.Timeout)
                    currentFloor._2 ! Floor.FloorReached(elevatorID, context.self)
                    //floorCallRequests -= ((nextFloor,floors(nextFloor)))
                    floorCallRequests = floorCallRequests.filterNot { case (floorId, _) => floorId == currentFloor._1 }
                    passengerCallRequests = passengerCallRequests.filterNot { case (floorId, _) => floorId == currentFloor._1 }
                    for(passenger <- passengerList) {
                        if(passenger._1 == currentFloor._1) {
                            passenger._2 ! Passenger.Arrived(currentFloor._2)
                            passengerList = passengerList.filterNot { case (floorId, _) => floorId == currentFloor._1 }
                        }
                    }
                    
                    waiting
            }
        }
    }

    private def nextFloorByPolicy: Int = {
        if(floorCallRequests.isEmpty) passengerCallRequests.head._1
        else if(passengerCallRequests.isEmpty) floorCallRequests.head._1
        else if(Math.abs(floorCallRequests.head._1 - currentFloor._1) < Math.abs(passengerCallRequests.head._1 - currentFloor._1)) floorCallRequests.head._1
        else passengerCallRequests.head._1
    }

    private def setFloors: Behavior[Elevator.Command] = {
        Behaviors.receiveMessage {
            case Elevator.SetFloors(floors) =>
                this.floors = floors
                currentFloor = (0, floors.head)
                idle
        }
    }
}