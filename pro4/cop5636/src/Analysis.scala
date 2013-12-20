import scala.swing._
import scala.swing.event._
import java.io.File;
import java.io._
import scala.io.Source
import javax.swing.table._
import scala.collection.immutable
import scala.sys.process._

class MyTableModel( var rowData: Array[Array[Any]], val columnNames: Seq[String] ) extends AbstractTableModel {
  override def getColumnName( column: Int) = columnNames(column).toString

  def getRowCount() = rowData.length

  def getColumnCount() = columnNames.length

  def getValueAt( row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]

  override def isCellEditable( row: Int, column: Int) = false

  override def setValueAt( value: Any, row: Int, col: Int) {
    rowData(row)(col) = value
  }

  def addRow( data: Array[AnyRef]) {
    rowData ++= Array(data.asInstanceOf[Array[Any]])
  }
}

class MultiSheet(file1: String, file2: Array[String]) extends ScrollPane{
	//preferredSize  = new Dimension( 400, 300 )
	val tableModel = new MyTableModel( Array[Array[Any]](), List("Clock", "From","To", "Message") )
    val table      = new Table( 20, 4 ) { model = tableModel }

	// set column width
	table.peer.getColumnModel.getColumn(0).setPreferredWidth(20)
	table.peer.getColumnModel.getColumn(1).setPreferredWidth(20)
	table.peer.getColumnModel.getColumn(2).setPreferredWidth(20)
	table.peer.getColumnModel.getColumn(3).setPreferredWidth(170)

   // Parse input file line by line
    val pattern = """\[(\d+)\]:\s\((\d+)\)\s\W\W\s\((\d+)\):\s(.*)""".r // define regex
    for(line: String <- Source.fromFile(file1+".log").getLines()){
	    val pattern(clock, from, to, msg) = line
	    if((from == file1 && file2.contains(to)) || (file2.contains(from) && to == file1)){
	    	tableModel.addRow( List(clock, from, to, msg).toArray )
	    }
    }

    viewportView = table
    rowHeaderView = new ListView( (1 to tableModel.getRowCount) map (_.toString)){
      fixedCellWidth = 30
      fixedCellHeight = table.rowHeight
    }

}

class TwoNode(file1: String, file2: Array[String]) extends Frame{
	title = "Log Message Between Two Nodes"

	val sheet1 = new BoxPanel(Orientation.Vertical){
	  contents += new Label("Node: "+file1)
	  contents += new MultiSheet(file1, file2)
	  border = Swing.EmptyBorder(30, 30, 10, 30)
	}

	val sheet2 = new BoxPanel(Orientation.Vertical){
	  contents += new Label("Node: "+file2.head)
	  contents += new MultiSheet(file2.head, Array[String](file1))
	  border = Swing.EmptyBorder(30, 30, 10, 30)
	}

	contents = new BorderPanel {
      import BorderPanel.Position._
      layout(sheet1) = West
      layout(sheet2) = East
      val b = new Button(Action("Close"){ close })
      layout(b)= South
    }
}

class MultiNode(file1: String, file2: Array[String]) extends Frame{
	title = "Log Message Between Two Nodes"

	var nodes : String=""
	for(t<-file2)
	  nodes += t + ", "

	val sheet1 = new BoxPanel(Orientation.Vertical){
	  contents += new Label("Node: "+file1+" [ communication with nodes "+nodes+" ]")
	  contents += new MultiSheet(file1, file2)
	  border = Swing.EmptyBorder(30, 30, 10, 30)
	}

	contents = new BorderPanel {
      import BorderPanel.Position._
      layout(sheet1) = Center
      val b = new Button(Action("Close"){ close })
      layout(b)= South
    }
}

class MultiNodeQuery(multi: Boolean) extends Dialog{
	title = "Analyze messages among multiple nodes"
	val num = new File("./").listFiles.filter(_.getName.endsWith(".log")).size // number of nodes
	val label = new Label("there are "+num+" [ 1- "+ num+" ] nodes in total")

	val dialog = new GridBagPanel { grid =>
      import GridBagPanel._
      preferredSize = new Dimension( 350, 150 )

      val c = new Constraints
      c.fill = Fill.Horizontal

      //node1
      c.grid = (1,2)
      layout(new Label("Node1")) = c
      c.grid = (1,3)
      val input1 = new TextField
      input1.preferredSize = new Dimension( 10, 30)
      c.insets = new Insets(0,0,0,20);  //top padding
      layout(input1) = c

       //node2
      c.grid = (5,2)
      // two nodes or multiple nodes
      if(multi)  layout(new Label("Multiple Nodes(separate by ',')")) = c
      else	layout(new Label("Node2")) = c

      c.grid = (5,3)
      val input2 = new TextField
      input2.preferredSize = new Dimension( 20, 30 )
      layout(input2) = c

      //button apply
      c.grid = (1,7)
      val b1 = new Button(Action("Apply"){
    	  val file1 = input1.text
    	  val file2 = input2.text.split(",")
    	  var flag = true
    	  for(t <- file2){
    	    val f2 = new File(t+".log")
    	     if(! f2.exists()){
    	       Dialog.showMessage(this, "please input valid nodes", "wrong nodes")
    	       flag = false;
    	     }
    	  }

    	  val f1 = new File(file1+".log")
    	  if(! f1.exists){
    	    Dialog.showMessage(this, "please input valid nodes", "wrong nodes")
    	    flag = false;
    	  }
    	  if(flag){
    	     if(multi) new MultiNode(file1, file2).open
    	     else new TwoNode(file1, file2).open
    	  }
      })
      b1.preferredSize = new Dimension( 50, 50 )
      c.insets = new Insets(20,0,0,20);  //top padding
      layout(b1) = c

      //button cancel
      c.grid = (5,7)
      val b2 = new Button(Action("cancel"){
        close()
      })
      b2.preferredSize = new Dimension( 50, 50 )
      c.insets = new Insets(20,0,0,20);  //top padding
      //b2.border = Swing.EmptyBorder(100, 30, 10, 10)
      layout(b2) = c

      border = Swing.EmptyBorder(20, 50, 50, 50)
	}

	contents = new BorderPanel {
		import BorderPanel.Position._
		layout(dialog) = Center
	    layout(label) = North

  }
}

class OverView{
  val num = new File("./").listFiles.filter(_.getName.endsWith(".log")) // number of nodes

  var table : Map[(Int,Int,String), Int] = Map()

  val pattern = """\[(\d+)\]:\s\((\d+)\)\s\W\W\s\((\d+)\):\s(.*)""".r // define regex
  num foreach(x =>
	  for(line: String <- Source.fromFile(x).getLines()){
	      val pattern(clock, from, to, msg) = line
	      var value = table.getOrElse((from toInt, to toInt ,msg), 0);
	      if(value != 0) table = table - ((from toInt, to toInt ,msg));
	      table = table + (( (from toInt, to toInt ,msg), value+1 ))
	    }
  )

  var sum : Int =0;  // the # of messages in total
  table foreach( sum += _._2 )
  //println("total message = " + sum)

  var lost : Int = table.filter(_._2 % 2 != 0).size ; // the # of lost messages
  //println("lost messages = " + lost)

  var green : Set[(Int, Int)] = Set()
  var red : Set[(Int,Int)] = Set()
  table.filter(_._2 % 2 == 0).keySet foreach(x => green = green + (( x._1, x._2)))
  table.filter(_._2 % 2 != 0).keySet foreach(x => red = red + ((x._1, x._2 )))

  var image = new FileWriter("overview.dot");
  image.write("digraph overview { \n");
  image.write("labelloc = \"t\" ; \n");
  image.write("label = \"there are "+sum+" messages in total, "+ lost +" messages lost \"; \n ")
  green foreach (x => image.write((x._1) + " -> " + (x._2) +"  [color = green] ; \n"))
  red foreach (x => image.write((x._1) + " -> " + (x._2) +"  [color = red] ; \n"))
  image.write("}");
  image.close();

  val cmd = "dot -Tpng overview.dot" #> new File("./overview.png")
  cmd.!
  "eog overview.png".!

  Dialog.showMessage(null, "there are "+sum+" messages in total, "+ lost +" messages lost ")

}

object App extends SimpleSwingApplication{
  def top = new MainFrame {
    title = "LogAnalysis"
    contents =  new GridBagPanel{
       import GridBagPanel._
       val c = new Constraints
       c.grid = (0,0)
       val b1 = new Button(Action("Analyze messages between two node      "){new MultiNodeQuery(false).open} )
       c.insets = new Insets(0,0,20,0);  //top padding
       layout(b1) = c

       val b2 = new Button(Action("Analyze messages among multiple nodes"){ new MultiNodeQuery(true).open})
       c.grid = (0,1)
       c.insets = new Insets(0,0,20,0);  //top padding
       layout(b2) = c

       val b3 = new Button(Action("Overview"){ new OverView() })
       c.grid = (0,2)
       c.insets = new Insets(0,0,20,0);  //top padding
       layout(b3) = c
       border = Swing.EmptyBorder(20, 50, 20, 50)
    }
  }
}