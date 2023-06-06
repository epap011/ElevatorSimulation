import scala.util.{Try,Success,Failure}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

object Main extends App {

    val elevatorsNumTry:Try[Int] = toInt(args(0))
    val floorsNumTry   :Try[Int] = toInt(args(1))
    val peopleNumTry   :Try[Int] = toInt(args(2))

    if(elevatorsNumTry.isFailure 
    || floorsNumTry.isFailure
    || peopleNumTry.isFailure) {
        println("[Error] Please provide 3 integer arguments")
        System.exit(1)
    }

    val elevatorsNum = elevatorsNumTry.get
    val floorsNum    = floorsNumTry.get
    val peopleNum    = peopleNumTry.get

    if(elevatorsNum < 1 || floorsNum < 1 || peopleNum < 1) {
        println("[Error] Please provide 3 positive integer arguments")
        System.exit(1)
    }

    def toInt(s: String): Try[Int] = Try(Integer.parseInt(s.trim))
}