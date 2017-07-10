package torje

import scala.io.Source
import java.io.File
import collection.mutable.HashMap
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io._

object Main extends App {

case object ShutDown  
  
/*parsing du scenario définissant la surface , les tondeuses et leurs mouvements */  
//val parser = new Parser 
val system = ActorSystem("torje")

//val src = Source.fromFile(getClass.getResource("/mesg111216.txt").getPath).getLines//.mkString("\n");

//val mesg = new MesgFile(getClass.getResource("/mesg111216.txt").getPath);

//print("dir Ressource:"+getClass.getResource("/"))

//print(scanMesgFile(getClass.getResource("/").getPath).map(_.getName()).foreach(println))

//println(getClass.getResource("/").getPath+"rje.txt")
//val file = new File(getClass.getResource("/").getPath+"rje.txt"/*fileName*/)
//val bw = new BufferedWriter(new FileWriter(file))
//bw.write("test")
//bw.close()

myTest(getClass.getResource("/").getPath)

//scanMesgFile(getClass.getResource("/").getPath).foreach(x => println(x.getName))
//print(scanTextFile(getClass.getResource("/").getPath).apply(0).getName)

//system.terminate

def myTest(folder:String) = {
 

  val mesgFiles = scanMesgFile(folder).map(x => new QueryMesgFile(pathToFile(x)/*,umids*/))//.filter(y => y.res.length>0);

  val mesgFile = mesgFiles.apply(0)
  
  val textFiles = scanTextFile(folder).map(x => new QueryTextFile(pathToFile(x)/*,umids*/))//.filter(y => y.res.length>0);

  val textFile = textFiles.apply(0)
  
  //textFile.contentIterator
  
  for { i <- textFile.contentIterator ; line = textFile.splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(textFile.posUmid) } 
  { 
    val text = line.get(textFile.posDataBlock).replace("\\r\\n","\r\n")    
    mesgFile.actorRef ! (umid,text)
  }

  mesgFile.actorRef ! ShutDown
  
  /*val myIt = mesgFile.res
  myIt.foreach(x => println(x._1))
  println("nb:"+myIt.length)
  println(myIt.apply(0)._2.apply(mesgFile.posSender))
  
  val myIt2 = mesgFile.myMap
  println(myIt2.contains("3114B129FE159B53"))
  */
  
  //mesgFile.actorRef ! "hello"
  //mesgFile.actorRef ! "hello"
  //mesgFile.actorRef 
  
  //system.stop(mesgFile.actorRef)
  
}

/* print final positions of mowers */  
//parser.parse(parser.exprAll,src).getOrElse(List()).foreach(println)

trait QueryFile extends ArchiveFile {
  val umids:List[String] 
  lazy val myMap = new HashMap[String,Array[String]]()
  lazy val res = (for { i <- contentIterator ; line = splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(posUmid)/*; if umids.exists(_==umid)*/ } yield (umid,line.get)).toList

  for { i <- contentIterator ; line = splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(posUmid) } 
  {myMap += (umid -> line.get)}
  
}

class QueryMesgFile(file:String/*,u:List[String]*/) extends MesgFile(file) with QueryFile  {
  override val umids = List.empty/*u*/;
  
  class ActorMesgFile() extends Actor{
    def receive = {
    case (umid : String ,text : String) => QueryMesgFile.this.writeMessage(umid,text)
    case "hello" => println("hello back at you")
    case ShutDown => QueryMesgFile.this.writerRef ! ShutDown
    case _       => println("huh?")
  }
  }
  
  class WriterRje(/*fileName:String*/) extends Actor{
    
    //val fileRje = new File(getClass.getResource("/").getPath+"rje.txt"/*fileName*/)
    val fileRje = new File(file+".rje"/*fileName*/)
    //val bw = new BufferedWriter(new FileWriter(fileRje))
    val pw = new PrintWriter(fileRje)
     def receive = {
    case (text:String) => {
      println(text)  
      pw.write(text)
      pw.flush()
    }
    case ShutDown => {
      system.terminate
    }
  }
    override def postStop() = {
    pw.flush(); 
    pw.close();
  }
  }
   
  def writeMessage(umid: String, text:String) = {
    
   val lineMesg = myMap.apply(umid)
    
   //val sens = "I"   
    val sens = lineMesg(posSens) match {                     
      case "OUTPUT" => "O"                     
      case "INPUT" => "I"                     
      case _ => "?"                   
    }
   val sender = lineMesg(posSender)
   val receiver = lineMesg(posReceiver)
   val mt = lineMesg(posMt)
   val priority = lineMesg(posNetworkPriority) match {
                     case "PRI_URGENT" => "U"
                     case "PRI_NORMAL" => "N"
                     case "PRI_SYSTEM" => "S"
                     case _ => "N"
                   }

   val field108 = Some(lineMesg(posField108)).filterNot(_.isEmpty).map(v => "{108:"+v+"}")
   val field119 = Some(lineMesg(posField119)).filterNot(_.isEmpty).map(v => "{119:"+v+"}")

   val nonDeliveryWarningFlag = lineMesg(posNonDeliveryWarning)=="TRUE" 
   val deliveryNotifFlag = lineMesg(posDeliveryNotif)=="TRUE" 

   val deliveryMonitory = (nonDeliveryWarningFlag,deliveryNotifFlag) match {
                           case (true,false) => "1"
                           case (false,true) => "2"
                           case (true,true) => "3"
                           case _ => ""
                         }

   val data = text
 
   val bloc1 = "{1:F01" + sender + repeatChar('0',9) + "}"
   val bloc2 = "{2:" + sens + mt + receiver + priority + "}"

   val bloc3 = (field108,field119) match {
                 case (None,None) => ""
                 case _ => "{3:" + field108.getOrElse("") + field119.getOrElse("") + "}"
                }

   val bloc4 = "{4:" + data + "\r\n-}"  

   print("Umid["+umid+"] done\n")
   
   writerRef ! (bloc1 + bloc2 + bloc3 + bloc4) 
  
  }
  
  val writerRef = system.actorOf(Props(classOf[WriterRje], this))
  val actorRef = system.actorOf(Props(classOf[ActorMesgFile], this))
  
}

class QueryTextFile(file:String/*,u:List[String]*/) extends TextFile(file) with QueryFile {
  override val umids =List.empty/*u*/;
}

def scanMesgFile(folder:String) = {
 new File(folder).listFiles.filter(file => file.getName.startsWith("mesg") && file.getName.endsWith(".txt")); 
}

def scanTextFile(folder:String) = {
 new File(folder).listFiles.filter(file => file.getName.startsWith("text") && file.getName.endsWith(".txt")); 
}

def pathToFile(f:File) = f.toString.replace("\\","/")

//val mesgFile = QueryMesgFile(pathToFile(x))



 def repeatChar(c:Char,nb:Int):String = nb match {
                  case n if n>0 => c + repeatChar(c,nb-1); 
                  case _ => ""; 
}


trait ArchiveLine{
  val posUmid:Int
  def getIndexFromHeader(s:String):Int
}


def splitLine(s:String) = s.split("\",").map(_.drop(1))
 
class TextFile(file:String) extends ArchiveFile(file) with TextLine 
class MesgFile(file:String) extends ArchiveFile(file) with MesgLine

trait MesgLine extends ArchiveLine {
  val posSender = getIndexFromHeader(":E:mesg_sender_swift_address")
  val posReceiver = getIndexFromHeader(":E:mesg_receiver_swift_address")
  val posMt = getIndexFromHeader(":E:mesg_type")
  val posTrnRef = getIndexFromHeader(":E:mesg_trn_ref")
  val posField108 = getIndexFromHeader(":E:mesg_user_reference_text") 
  val posSens = getIndexFromHeader(":E:mesg_sub_format")
  override val posUmid = getIndexFromHeader("mesg_s_umid")
  val posNetworkPriority = getIndexFromHeader(":E:mesg_network_priority")
  val posNonDeliveryWarning = getIndexFromHeader(":E:mesg_delv_overdue_warn_req")
  val posDeliveryNotif = getIndexFromHeader(":E:mesg_network_delv_notif_req")
  val posField119 = getIndexFromHeader(":E:mesg_mesg_user_group")
}

 abstract class ArchiveFile(f:String) extends ArchiveLine {

 private val file = f
 val header = loadFile._1
 def contentIterator = loadFile._2
 val headerLength = header.length
 
 
 private def loadFile:(Array[String],Iterator[String]) = {
        print("Archive["+f+"] researched\n")
        val itFile = scala.io.Source.fromFile(file).getLines
        (splitLine(itFile.take(1).toList(0)),itFile)
 }
 
 print("Archive["+f+"] found\n")


 override def getIndexFromHeader(keyword:String) = {
  val i = header.indexWhere(_==keyword)
  if (i<0) print ("header field["+keyword+"] not found \n")
  i }
  def splitAndCheckLine(s:String) = {
  val tab = s.split("\",")
  tab.length match {
       case i if (i==headerLength) => Some(tab.map(_.drop(1)))
       case _ => None
      }
  }
 

 }

trait TextLine extends ArchiveLine {
  override val posUmid = getIndexFromHeader("text_s_umid")
  val posDataBlock = getIndexFromHeader(":E:text_data_block")
}

}
