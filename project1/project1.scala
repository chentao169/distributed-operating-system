import scala.math._
import akka.actor._ 
import akka.routing.RoundRobinRouter

sealed trait Message
case object Begin extends Message
case class Work(begin: Long,  length: Long, k: Long) extends Message
case class Result(begin: Long) extends Message
case object Finished extends Message

class Master(N: Long, K: Long, np: Int) extends Actor {
    var nrOfFinished: Int = 0
    
    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(np)), name = "workerRouter")
          
    def receive = {
      case Begin => {{for (i <- 1 to (np-1)) workerRouter ! Work(1+(i-1)*N/np, N/np, K)}
        workerRouter ! Work(1+(np-1)*(N/np), N-(np-1)*(N/np), K)
      }
      case Result(b) => println(b)
      case Finished => {
    	  	nrOfFinished += 1
    	  	if(nrOfFinished == np) 
    	  		context.system.shutdown()        
      }
    }
  }

class Worker extends Actor {  
    def receive = {
      case Work(b, n, k) => {
	          for (i <- b to b+n-1 )  {
	             val ai = ((i+k-1) * (i+k) *(2*(i+k)-1) - (i-1) * i * (2*i-1))/6
	             val root = round(sqrt(ai))
	             if( ai == (root * root))
	               sender ! Result(i)
	          }
	          sender ! Finished
      }
    }
}
  
  
object project1 {
  def main(args: Array[String]) {
    val N = if (args.length > 0) args(0) toInt else 1000000  // size of problem
    val k = if (args.length > 1) args(1) toInt else 24    // length of sum sequence
    val np = if (args.length > 2) args(2) toInt else 4        // # of subproblems (partitions)
    
    if (np < 2) {
      println("Please indicate more than one workers!")
      exit
    }
    
    val system = ActorSystem("SquareSystem")    
    val master = system.actorOf(Props(new Master(N, k, np)), name = "master")    
    master ! Begin
  }
  
}
