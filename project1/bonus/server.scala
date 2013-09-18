import java.net.InetAddress
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.AddressFromURIString
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.kernel.Bootable
import akka.remote.routing.RemoteRouterConfig
import akka.routing.RoundRobinRouter
import akka.actor.ActorRef

class MasterAppliation(N:Long, K:Long, np:Int) extends Bootable{
	val hostname = InetAddress.getLocalHost.getHostName
    val config = ConfigFactory.parseString(
		  s"""
		  	akka{
		  		actor{
		  			provider = "akka.remote.RemoteActorRefProvider"
		  		}
		  		remote{
		  			enabled-transports = ["akka.remote.netty.tcp"]
		  			netty.tcp{
						hostname = "$hostname"
						port = 2551
					}
				}     
    	}""")
		  
    val system = ActorSystem("master", ConfigFactory.load(config))
    
    val addresses = for(i <-1 to 10) 
	   yield AddressFromURIString("akka.tcp://remotesys@lin114-"+"%02d".format(i)+".cise.ufl.edu:2552")

   val routerRemote = system.actorOf(Props[RemoteMaster].withRouter(
		   RemoteRouterConfig(RoundRobinRouter(10), addresses)))
    
    val master = system.actorOf(Props(new Master(routerRemote)), "master")   
         
    def dosomething = master ! Begin(N, K, np)
    
    def startup() {}

    def shutdown() {
    	system.shutdown()
    }   
}

class Master(routerRemote: ActorRef) extends Actor {   
   var finished = 0 
   
   def receive = {
     case Begin(n,k,np)=>{
       println("distribute task to remote actors")
       for(i <- 1 until 10) routerRemote ! Work(1+(i-1)*(n/10), n/10, k, np, self)
       routerRemote ! Work(1+9*(n/10), n-9*(n/10), k, np, self)   
     }
     case Result(n) => println(n)
     
     case Finished => {
       finished +=1
       if(finished == 10){
         routerRemote ! ShutDown
         context.system.shutdown()
       }          
     }          
   }  
}

object server {
  def main(args: Array[String]) {
    val N = if (args.length > 0) args(0) toInt else 1000000l		 // size of problem
    val K = if (args.length > 1) args(1) toInt else 24l    	 // length of sum sequence
    val np = if (args.length > 2) args(2) toInt else 4			// # of subproblem in each remote worker
    
    new MasterAppliation(N, K, np).dosomething
    println("master node start!")
  }
}
