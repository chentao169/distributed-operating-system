import scala.math._
import scala.actors._
import scala.actors.Actor._
import scala.util.Random
//import scala.concurrent.Future

sealed trait Message
case object Rumor extends Message
case object Stop extends Message
case class Remove(s:Actor) extends Message
case class Init(neighbors:Array[Node], master:Actor) extends Message
case object Loop extends Message

abstract class Node extends Actor with Actorlog{
  var neighbors : Array[Node] = Array.empty
  val rand = new Random
  var master : Actor = null

  def init(neighbors:Array[Node], master: Actor){
    //this.id = id
    this.neighbors = neighbors
    this.master = master
    rand.setSeed(System.currentTimeMillis() ^ (neighbors.hashCode toLong))
  }

  def randNeighbor() : Actor = if(neighbors.isEmpty) self else neighbors(rand.nextInt(neighbors.size))

  def removeNeighbor(x:Actor) = neighbors = neighbors.filter( _!= x)

}
class GossipNode extends Node{
  private var count:Int = 0;
  private val max: Int = 2;

  def act() {
    loop{
      react {
        case Init(neighbors, master) => init(neighbors, master)
        case Rumor => {
	    	count+=1;
	    	if(count== max){
	    		neighbors.foreach(ref =>ref ! Remove(self))
	    		master ! Stop
	    		exit()
	    	}
		    if(count==1) self ! Loop
	        }
	    case Loop => if(count < max){ randNeighbor() ! Rumor; Thread.sleep(2); self ! Loop}
	    case Remove(s) => removeNeighbor(s)
	    case _ => println("wrong message")
      }
    }
  }

}

abstract class NetWork(numNodes: Int, algorithm: String) extends Actor with Actorlog{
  var count = 0
  var index : Int = 1
  var x :Int =1
  val nodes = Array.fill(numNodes)(new GossipNode)
  val rand = new Random(System.currentTimeMillis())
  val randomNode = nodes(rand.nextInt(numNodes))
  var begin : Long= 0

 def findNeighbors(x:Int): Array[Node]

  def act() {
    nodes foreach(x => {link(x); x start})
    nodes.foreach(ref => { ref ! Init(findNeighbors(index-1), self); index +=1})
    loop{
	  react {
	    case Rumor =>
	    	randomNode ! Rumor
	    	begin = System.currentTimeMillis
	    case Stop => count += 1;
	    	if(count >= numNodes) {
	    	  println("numNodes = "+ numNodes+", topology = "+ algorithm +", consumes "+(System.currentTimeMillis-begin)+" ms")
	    	  exit()
	    	}
	  	}
    }
  }
}

class Full(numNodes: Int, algorithm: String) extends NetWork(numNodes, algorithm){
  def findNeighbors(x:Int): Array[Node]=
   Array.range(0, numNodes) filter (_!=x) map (nodes(_))
}

object project2 {
	def main(args: Array[String]){
	   val network = new Full(16, "gossip") start;
	   network ! Rumor
	   println("start !")
	}

}