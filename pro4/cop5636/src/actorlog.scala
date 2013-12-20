import scala.actors._
import scala.actors.Actor._
import java.io._

case class WrapMessage(fromid: Int, clock: Int, msg: Any)

object Actorlog{
  private var ID : Int = 0;
  private def getID : Int = { ID += 1; ID }
}

trait Actorlog extends Actor {
  val id = Actorlog.getID;
  val logname = id + ".log";
  var clock: Int = 0;

  writeToFile(logname, "", false)

  private def writeToFile(filename: String, info: String, append: Boolean=true) ={
    var file = new FileWriter(filename, append);
    try{
      file.write(info);
    }finally{
      file.close();
    }
  }

  private def receiveUpdate(time: Int)={
    clock = (clock max time)+1
  }

  override def react(handler: PartialFunction[Any, Unit]) = {
    super.react {
      case WrapMessage(fromId: Int, fromClock: Int, msg: Any) =>   // unwrap
        receiveUpdate(fromClock)
        writeLog("["+ clock +"]"+ ": ("+ fromId +") -> ("+ this.id +"): "+ msg+"\n");
        handler.apply(msg)
      case _ =>
        println("something wrong~")
        throw new RuntimeException   // should not happen since ! is overridden
    }
  }

  // pattern matching
  private def getOrElse[T](get: Actorlog => T, default: T): T =
    self match {
    	case x: Actorlog => get(x)
    	case _ => default
    }

  def writeLog(data: String) = getOrElse(x => writeToFile(x.logname, data), ())

  private def senderId = getOrElse(_.id, 0)

  def sendUpdate() =  { getOrElse(x => x.clock+=1, ()) }

  def sendClock = getOrElse(_.clock, 0)

  override def !(msg: Any) = {
    sendUpdate()
    val (fromId, senderClock) = (senderId, sendClock);
    writeLog("[" + senderClock + "]: (" + fromId + ") -> (" +this.id+ "): " +msg+ "\n");
    super.! (WrapMessage(senderId, senderClock, msg));
  }

}