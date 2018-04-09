package torje

import scala.io.Source
import java.io.File

import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction

import collection.mutable.HashMap
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.io._
//import scala.xml.XML

//import com.ximpleware.{VTDGen, VTDNav, AutoPilot}

object Main extends App {

case object ShutDown
//case object NewFileFIN
//case object NewFileMX

case class FIN(s:String)
case class MX(s:String)

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

//myTest(getClass.getResource("/").getPath)
println("Scanning dir["+System.getProperty("user.dir")+"]")

//"D:\\myData\\scala_vrac-master\\scala_vrac-master\\torje\\mesg1706010000P.txt".split("\\\\").foreach(println)

val path = "D:\\myData\\scala_vrac-master\\scala_vrac-master\\torje\\mesg1706010000P.txt"
val filename = path.split("\\\\").last
val filenameWithout = filename.split("\\.").init.mkString


//println ("D:\\myData\\scala_vrac-master\\scala_vrac-master\torje\\mesg1706010000P.txt".split("\\").init.mkString)
//scanMesgFile(System.getProperty("user.dir")).foreach(println(_))

myTest(System.getProperty("user.dir"))

//scanMesgFile(getClass.getResource("/").getPath).foreach(x => println(x.getName))
//print(scanTextFile(getClass.getResource("/").getPath).apply(0).getName)

//system.terminate
//Await.ready(system.whenTerminated, Duration(1, TimeUnit.MINUTES))
system.terminate
system.whenTerminated
println("---------------------end----------------------------")

def myTest(folder:String) = {

  val mesgFiles = scanMesgFile(folder)//.map(x => new QueryMesgFile(pathToFile(x)/*,umids*/))//.filter(y => y.res.length>0);

  mesgFiles.foreach { pathMesg =>
   val mesgFile = new QueryMesgFile(pathToFile(pathMesg))
   val filename = mesgFile.file.split("\\\\").last
   val filenameWithout = filename.split("\\.").init.mkString
   val textFile = new QueryTextFile(filenameWithout.replace("mesg","text")+".txt")

   mesgFile.generateRje(textFile)

   mesgFile.actorRef ! ShutDown
  }


  //val mesgFile = mesgFiles.apply(0)



}

trait QueryFile extends ArchiveFile {
  val umids:List[String]

  lazy val res = (for { i <- contentIterator ; line = splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(posUmid)/*; if umids.exists(_==umid)*/ } yield (umid,line.get)).toList

}

class QueryMesgFile(file:String) extends MesgFile(file) with QueryFile  {
  override val umids = List.empty;
  lazy val myMap = new HashMap[String,Array[String]]()
  var countWrite=0
  var len  = 0//contentIterator.size

  def generateRje(textFile:QueryTextFile) {

    var sizeMap=0;
    val filename = file.split("/").last
    println("generateRje for ["+filename+"]")
    def parseTextFile() {
      for { i <- textFile.contentIterator ; line = textFile.splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(textFile.posUmid) }
      {
        val text = line.get(textFile.posDataBlock).replace("\\r\\n","\r\n")
        actorRef ! (umid,text)
      }
    }

    for { i <- contentIterator ; line = splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(posUmid) }
    {
      myMap += (umid -> line.get)
      sizeMap += 1
      len += 1
      if (sizeMap>100000) {
        parseTextFile()
        myMap.clear()
        sizeMap = 0
        //writerRef ! NewFileFIN
      }
  }
  if (sizeMap>0) parseTextFile()
}

  class ActorMesgFile() extends Actor{
    def receive = {
    case (umid : String ,text : String) => QueryMesgFile.this.writeMessage(umid,text)
    case "hello" => println("hello back at you")
    case ShutDown => {
      QueryMesgFile.this.writerRef ! ShutDown
      context.stop(self)
    }
    case _       => println("huh?")
  }
  }

  class WriterRje(/*sizeMax:Int*/) extends Actor{

    val sizeMax = 100000000
    var nbFINRje = 0
    var nbMXRje = 0

    //var firstInFileFIN = true
    //var firstInFileMX = true

    var lenFileFIN = 0
    var lenFileMX =0

    def newFileFIN_Rje = new File(file.split("\\.").init.mkString.replace("mesg","MT_")+".part"+nbFINRje+".rje")
    var pwFIN = new PrintWriter(newFileFIN_Rje)

    def newFileMX_Rje = new File(file.split("\\.").init.mkString.replace("mesg","MX_")+".part"+nbMXRje+".rje")
    var pwMX = new PrintWriter(newFileMX_Rje)

     def receive = {
    case FIN(text:String) => {
      if ((lenFileFIN+text.length+1)>=sizeMax) newFileFIN
      if (lenFileFIN!=0) {
         pwFIN.write("$")
         lenFileFIN += 1
       }
      pwFIN.write(text)
      lenFileFIN += text.length
      countWrite=countWrite+1;
      if ((countWrite % 10000) == 0) println(countWrite+" msg done")
      pwFIN.flush()
    }

    case MX(text:String) => {

      if ((lenFileMX+text.length+1)>=sizeMax) newFileMX
      if (lenFileMX!=0) {
         pwMX.write("$")
         lenFileMX += 1
       }
      pwMX.write(text)
      lenFileMX += text.length
      countWrite=countWrite+1;
      if ((countWrite % 10000) == 0) println(countWrite+" msg done")
      pwMX.flush()
    }

    case ShutDown => {
      //myMap.foreach { case (key, value) => println(">>> key=" + key + ", value=" + value.mkString(",")) }
      println("nb Origin["+len +"] nb Writed["+countWrite+"]")
      context.stop(self)
      //system.terminate
    }
  }

  def newFileFIN = {
    nbFINRje += 1
    pwFIN.flush();
    pwFIN.close();
    pwFIN = new PrintWriter(newFileFIN_Rje)
    lenFileFIN = 0
  }

  def newFileMX = {
    nbMXRje += 1
    pwMX.flush();
    pwMX.close();
    pwMX = new PrintWriter(newFileMX_Rje)
    lenFileMX = 0
  }

    override def postStop() = {
    pwFIN.flush();
    pwFIN.close();
    pwMX.flush();
    pwMX.close();
  }
  }

  def writeMessage(umid: String, text:String) = {

   myMap.get(umid).map(write)

   def write(lineMesg:Array[String]) {
     lineMesg(posFormatName) match {
       case "Swift" => writeFIN(lineMesg)
       case "Internal" => len -= 1
       case _ => writeMX("<SwInt:RequestPayload>"+text.replace("\\\"","\"")+"</SwInt:RequestPayload>\n")
       //case _ => writeMX(text.replace("\\\"","\""))
     }
   }

   def writeMX(xmlString:String) {

     //val withHeader  = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + xmlString;
     //val bytes = withHeader.getBytes("UTF-8");
     //val vg = new VTDGen();
     //vg.setDoc(bytes);
     //vg.parse(false);

     //val vn = vg.getNav();
     //val ap = new AutoPilot(vn);
     //ap.selectXPath("//Document");

     //var i = -1
     //while({i = ap.evalXPath; i} != -1) {
    //    val l = vn.getContentFragment()
    //    writerRef ! MX("<" + vn.toRawString(vn.getCurrentIndex()) + ">"
    //            + vn.toString(l.toInt, (l>>32).toInt)
    //            + "</" + vn.toRawString(vn.getCurrentIndex()) + ">")
    //    println(vn.toString(l.toInt, (l>>32).toInt))
    //    println(vn.toString(i)/*vn.toNormalizedString(i))
    // }
    // writerRef ! MX(xmlString.substring(idx )*/

     val idx = xmlString.indexOf("AppHdr>",xmlString.indexOf("AppHdr>")) + 7//max xmlString.indexOf("<Doc:Document")
     if (idx>7) writerRef ! MX(xmlString.substring(idx ) )
   }

   def writeFIN(lineMesg:Array[String]) {

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

   val bloc1 = "{1:F01" + sender + repeatChar('0',10) + "}"
   val bloc2 = sens match {
                    case "I" => "{2:" + sens + mt + receiver + priority + "}"
                    case "O" => "{2:" + sens + mt + repeatChar('0',10) + receiver + repeatChar('0',20) + priority + "}"
   }

   val bloc3 = (field108,field119) match {
                 case (None,None) => ""
                 case _ => "{3:" + field108.getOrElse("") + field119.getOrElse("") + "}"
                }

   val bloc4 = "{4:" + data + "\r\n-}"
   //print("Umid["+umid+"] done\n")

   writerRef ! FIN(bloc1 + bloc2 + bloc3 + bloc4)
}
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
  val posFormatName = getIndexFromHeader("mesg_frmt_name")

  val posRequestor = getIndexFromHeader("mesg_requestor_dn")
  //val posResponder = getIndexFromHeader("mesg_frmt_name")
  val posRequestType = getIndexFromHeader("mesg_request_type")
  val posService = getIndexFromHeader("mesg_service")

  val posSender = getIndexFromHeader("mesg_sender_swift_address")
  val posReceiver = getIndexFromHeader("mesg_receiver_swift_address")
  val posMt = getIndexFromHeader("mesg_type")
  val posTrnRef = getIndexFromHeader("mesg_trn_ref")
  val posField108 = getIndexFromHeader("mesg_user_reference_text")
  val posSens = getIndexFromHeader("mesg_sub_format")
  override val posUmid = getIndexFromHeader("mesg_s_umid")
  val posNetworkPriority = getIndexFromHeader("mesg_network_priority")
  val posNonDeliveryWarning = getIndexFromHeader("mesg_delv_overdue_warn_req")
  val posDeliveryNotif = getIndexFromHeader("mesg_network_delv_notif_req")
  val posField119 = getIndexFromHeader("mesg_mesg_user_group")
}

 abstract class ArchiveFile(f:String) extends ArchiveLine {

 val file = f
 val header = loadFile._1
 def contentIterator = loadFile._2
 val headerLength = header.length


 private def loadFile:(Array[String],Iterator[String]) = {
        //print("Archive["+f+"] researched\n")
        val decoder = Charset.forName("UTF-8").newDecoder()
        //decoder.onMalformedInput(CodingErrorAction.IGNORE)
        val itFile = scala.io.Source.fromFile(file)(decoder).getLines
        (splitLine(itFile.take(1).toList(0)),itFile)
 }

 //print("Archive["+f+"] found\n")


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
  val posDataBlock = getIndexFromHeader("text_data_block")
}

}
