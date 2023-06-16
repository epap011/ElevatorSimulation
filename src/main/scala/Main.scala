import scala.util.{Try,Success,Failure}
import akka.actor.typed.ActorSystem

object Main extends App {

    val elevatorsNumTry  :Try[Int] = toInt(args(0))
    val floorsNumTry     :Try[Int] = toInt(args(1))
    val passengersNumTry :Try[Int] = toInt(args(2))

    if(elevatorsNumTry.isFailure 
    || floorsNumTry.isFailure
    || passengersNumTry.isFailure) {
        println("[Error] Please provide 3 integer arguments")
        System.exit(1)
    }

    val elevatorsNum  = elevatorsNumTry.get
    val floorsNum     = floorsNumTry.get
    val passengersNum = passengersNumTry.get

    if(elevatorsNum < 1 || floorsNum < 1 || passengersNum < 1) {
        println("[Error] Please provide 3 positive integer arguments")
        System.exit(1)
    }

    val system: ActorSystem[Building.BuildingMessage] = ActorSystem(Building(), "building")
    system ! Building.StartBuilding(elevatorsNum, floorsNum, passengersNum)
    system ! Building.ActivateElevators

    Thread.sleep(1000*60)
    system.terminate()

    def toInt(s: String): Try[Int] = Try(Integer.parseInt(s.trim))
}