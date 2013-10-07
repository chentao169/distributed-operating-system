package target
import scala.math._
import akka.actor._
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.Future

sealed trait Message
case object Rumor extends Message
case object Stop extends Message
case class Remove(s:ActorRef) extends Message
case class sum(s:Double, m: Double) extends Message
case class Init(id:Int, neighbors:Array[ActorRef], master:ActorRef) extends Message
case object Loop extends Message
case class Ave(x: Double) extends Message

abstract class Node extends Actor{
  var id:Int =0
  var neighbors : Array[ActorRef] = Array.empty
  val rand = new Random
  var master : ActorRef = null
    
  def init(id:Int, neighbors:Array[ActorRef], master: ActorRef){
    this.id = id
    this.neighbors = neighbors
    this.master = master
    rand.setSeed(System.currentTimeMillis() ^ (neighbors.hashCode toLong))
  }
  
  def randNeighbor() : ActorRef = if(neighbors.isEmpty) self else neighbors(rand.nextInt(neighbors.size))
    
  def removeNeighbor(x:ActorRef) = neighbors = neighbors.filter( _!= x)
      
}
class GossipNode extends Node{
  private var count:Int = 0;
  private val max: Int = 10;
  
  def receive = {
    case Init(id, neighbors, master) => init(id, neighbors, master)
    case Rumor => {		
    	count+=1; 
    	if(count== max){    	 
    		neighbors.foreach(ref =>ref ! Remove(self)) 
    		master ! Stop
    		context.stop(self)
    	}
	    if(count==1) self ! Loop
    }	
    case Loop => if(count < max){ randNeighbor() ! Rumor; Thread.sleep(2); self ! Loop}
    case Remove(s) => removeNeighbor(s) 	
    case _ => println("wrong message")
  }
}

class PushsumNode extends Node{
  private var s:Double = 0
  private var w:Double = 1
  private val map: Array[Boolean] = Array(false, false, false)
  private var count:Int = 0
  
  override def init(id: Int, neighbors:Array[ActorRef], master: ActorRef){
    super.init(id, neighbors, master);
    s = id;
  }
  
  def receive = {
    case Init(id, neighobrs, master) => init(id, neighbors, master)
    case Rumor => s/=2.0; w /=2; randNeighbor ! sum(s, w)
    case sum(s1, w1) => {
      count += 1
      map(count%3) = abs(s/w-(s+s1)/(w+w1)) < 1e-10
      if(map forall(_== true)){
        neighbors.foreach(ref =>ref ! Remove(self))
        master ! Ave(s/w)
		context.stop(self)
      }else{
        s += s1; w += w1
        s /= 2.0; w /= 2.0
        randNeighbor ! sum(s, w)
      }
    }
	//case Loop => if(! map forall(_== true)){ randNeighbor ! sum(s, w)}
    case Remove(s) => removeNeighbor(s)
    case _ => println("wrong message")  
  }    
}

abstract class NetWork(numNodes: Int, algorithm: String) extends Actor{
  var count = 0
  var index : Int = 1
  val method = if(algorithm== "gossip") ()=>{context.actorOf(Props(new GossipNode))}
			   else ()=>{context.actorOf(Props(new PushsumNode))}
  var x :Int =1
  val nodes = Array.fill(numNodes)(method())
  val rand = new Random(System.currentTimeMillis())
  val randomNode = nodes(rand.nextInt(numNodes))  
  var begin : Long= 0
  
  // for grid and imperfectgrid topology
  val rows = List.range(math.sqrt(numNodes) toInt, 0, -1).find(numNodes%_==0).get
  val cols = numNodes/rows

  def findNeighbors(x:Int): Array[ActorRef]
  
  def receive = {    
    case Rumor => 
    	nodes.foreach(ref => { ref ! Init(index, findNeighbors(index-1), self); index+=1})
    	randomNode ! Rumor
    	begin = System.currentTimeMillis
    case Stop => count += 1; 
    	if(count >= numNodes) {
    	  println("numNodes = "+ numNodes+", topology = "+ algorithm +", consumes "+(System.currentTimeMillis-begin)+" ms")
    	  context.system.shutdown 
    	}   
	case Ave(x) => 
		println("numNodes = "+ numNodes+", topology = "+ algorithm +", consumes "+(System.currentTimeMillis-begin)+" ms, average = "+ x)  
		context.system.shutdown
  }  
}

class Full(numNodes: Int, algorithm: String) extends NetWork(numNodes, algorithm){
  def findNeighbors(x:Int): Array[ActorRef]= 
    nodes.filter(_!= nodes(x)) 
}

class Grid(numNodes: Int, algorithm: String) extends NetWork(numNodes, algorithm){
  def findNeighbors(x:Int): Array[ActorRef]={
      val v = x/cols
      val h = x%cols
      val r = Array((v-1,h),(v+1,h),(v,h-1),(v, h+1)).filter(ref=> ref._1>=0 &&ref._1<rows &&ref._2>=0 &&ref._2<cols)
      r.map(ref=>nodes(ref._1*cols+ref._2))     
  }  
}

class Line(numNodes: Int, algorithm: String) extends NetWork(numNodes, algorithm){
   def findNeighbors(x:Int): Array[ActorRef]=     
       Array(x-1, x+1).filter(ref=> ref>=0 && ref<nodes.length).map(ref=> nodes(ref))
}

class ImperfectGrid(numNodes: Int, algorithm: String) extends Grid(numNodes, algorithm){   
   override def findNeighbors(x:Int): Array[ActorRef]={     
      var result = super.findNeighbors(x)
      var tmp = nodes(rand.nextInt(numNodes))
      while(result contains tmp )
           tmp = nodes(rand.nextInt(numNodes))
      result :+ tmp
  }  
}

object project2 {
	def main(args: Array[String]){
	  val numNodes = args(0) toInt;
	  if(numNodes <= 0){ println("wrong naneNode"); exit()}
	  
	  val system = ActorSystem("GossipSimulator") 
	   
	  val algorithm = args(2)	  		 
	
	  val topology = args(1) match{
	    case "full" => system.actorOf(Props(new Full(numNodes, algorithm)), name = "full") 
	    case "grid" => system.actorOf(Props(new Grid(numNodes, algorithm)), name = "grid")
	    case "line" => system.actorOf(Props(new Line(numNodes, algorithm)), name = "line")
	    case "imperfectgrid" => system.actorOf(Props(new ImperfectGrid(numNodes, algorithm)), name = "imperfectgrid")
	    case _ => println("wrong topology"); exit()
	  }
	  topology ! Rumor     
	  
	}  
  
}
