import scala.math.round
import scala.math.sqrt
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.kernel.Bootable
import akka.routing.RoundRobinRouter
import java.net.InetAddress
import akka.actor.ActorRef

sealed trait Message
case class Begin(N:Long, K:Long, np:Int) extends Message
case class Work(begin: Long,  length: Long, k: Long, np:Int, master: ActorRef) extends Message
case class SubWork(begin: Long, length:Long, k:Long, master:ActorRef) extends Message
case class Result(begin: Long) extends Message
case object Finished extends Message
case object ShutDown extends Message

class RemoteMaster extends Actor {
    var nrOfFinished: Int = 0 
    var worker:Int = 0
    var master : ActorRef = _
    
    def receive = {
      case Work(b, n, k, np, self)=>{
        val workerRouter = context.actorOf(
        		Props[RemoteWorker].withRouter(RoundRobinRouter(np)), name = "workerRouter"+b)
        worker = np
        master = self
        for(i <- 1 until np)  workerRouter ! SubWork(b+(i-1)*(n/np), n/np, k, self)
          workerRouter ! SubWork(b+(np-1)*(n/np), n-(np-1)*(n/np), k, self)          
      	}      
      case Result(begin) => master ! Result(begin)
      case Finished => {
    	  	nrOfFinished += 1
    	  	if(nrOfFinished == worker)
    	  	  master ! Finished  		       
      }
      case ShutDown => context.system.shutdown()
    }
  }

class RemoteWorker extends Actor {  
    def receive = {
      case SubWork(b, n, k, sender) => {
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

class Remote extends Bootable{
  val hostname = InetAddress.getLocalHost.getHostName
  val config = ConfigFactory.parseString(
      s"""
      	akka {
      		actor {
      			provider = "akka.remote.RemoteActorRefProvider"
      		}
      		remote {
      			netty.tcp {
      				hostname = "$hostname"
      				port = 2552
      			}
      		}       		
      	}      
      """)
  val system = ActorSystem("remotesys", ConfigFactory.load(config))
       
  def startup() {}

  def shutdown() {
	  system.shutdown()
  }   
}

object client{
  def main(args: Array[String]) {
	  new Remote
	  println("remote system start!")
  }  
}
