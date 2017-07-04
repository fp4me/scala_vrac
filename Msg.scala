import java.io.File
import java.util.Properties
import java.io.FileInputStream
import java.io.FileWriter

object Msg {

class  ConfigProperties() {

  private val props:java.util.Properties = new java.util.Properties()
  props.load(new FileInputStream("config.properties"))
  def getParameter(key: String): String = Option(props.getProperty(key)) match {
			case Some(value) => value
			case None => "" }
}

def getConfigProperties : Option[ConfigProperties] = {
try { Some(new ConfigProperties) } catch { 
 case _ =>  Console.println("config file[config.properties] not found");
            None
            
 }
}


def scanMesgFile(folder:String) = {
 new File(folder).listFiles.filter(file => file.getName.startsWith("mesg") && file.getName.endsWith(".txt")); 
}

def scanTextFile(folder:String) = {
 new File(folder).listFiles.filter(file => file.getName.startsWith("text") && file.getName.endsWith(".txt")); 
}


def splitLine(s:String) = s.split("\",").map(_.drop(1))


trait ArchiveLine {
  val posUmid:Int
  def getIndexFromHeader(s:String):Int
}


abstract class ArchiveFile(f:String) extends ArchiveLine {

 private val file = f 

 print("Archive["+f+"] found\n")
 private def loadFile:(Array[String],Iterator[String]) = {
        val itFile = scala.io.Source.fromFile(file).getLines
        (splitLine(itFile.take(1).toList(0)),itFile)
 }

 val header = loadFile._1
 def contentIterator = loadFile._2
 val headerLength = header.length

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

trait QueryFile extends ArchiveFile {
  val umids:List[String] 
  lazy val res = (for { i <- contentIterator ; line = splitAndCheckLine(i) ; if line.isDefined ; umid = line.get(posUmid); if umids.exists(_==umid) } yield (umid,line.get)).toList
}

class QueryMesgFile(file:String,u:List[String]) extends MesgFile(file) with QueryFile {
  override val umids =u;
}

class QueryTextFile(file:String,u:List[String]) extends TextFile(file) with QueryFile {
  override val umids =u;
}

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

trait TextLine extends ArchiveLine {
  override val posUmid = getIndexFromHeader("text_s_umid")
  val posDataBlock = getIndexFromHeader(":E:text_data_block")
}



 def repeatChar(c:Char,nb:Int):String = nb match {
                  case n if n>0 => c + repeatChar(c,nb-1); 
                  case _ => ""; 
}

def pathToFile(f:File) = f.toString.replace("\\","/")

def generateMesgFromArchive(umids:List[String],folder:String) = {
 
  val mesgFiles = scanMesgFile(folder).map(x => new QueryMesgFile(pathToFile(x),umids)).filter(y => y.res.length>0);
  val textFiles = scanTextFile(folder).map(x => new QueryTextFile(pathToFile(x),umids)).filter(y => y.res.length>0);

  umids.foreach(u => { 
                       if (mesgFiles.flatMap(m => m.res).map(_._1).contains(u)==false)
                             sys.error("umid["+u+"] not found in mesg archive files")
		       if (textFiles.flatMap(m => m.res).map(_._1).contains(u)==false)
                             sys.error("umid["+u+"] not found in text archive files")
                     }
               )
 

  for { m <- mesgFiles.iterator; t <- textFiles.iterator ; lm <- m.res.iterator ;  ltOption = t.res.find(_._1==lm._1); if ltOption.isDefined; lt = ltOption.get } 
  yield { 
   val lineMesg = lm._2 
   val lineText = lt._2 

   val sens = "I"
   val sender = config.getParameter("SND_ADDR")  // lineMesg(m.posSender)
   val receiver = config.getParameter("RCV_ADDR") //lineMesg(m.posReceiver)
   val mt = lineMesg(m.posMt)
   val priority = lineMesg(m.posNetworkPriority) match {
                     case "PRI_URGENT" => "U"
                     case "PRI_NORMAL" => "N"
                     case "PRI_SYSTEM" => "S"
                     case _ => "N"
                   }

   val field108 = Some(lineMesg(m.posField108)).filterNot(_.isEmpty).map(v => "{108:"+v+"}")
   val field119 = Some(lineMesg(m.posField119)).filterNot(_.isEmpty).map(v => "{119:"+v+"}")

   val nonDeliveryWarningFlag = lineMesg(m.posNonDeliveryWarning)=="TRUE" 
   val deliveryNotifFlag = lineMesg(m.posDeliveryNotif)=="TRUE" 

   val deliveryMonitory = (nonDeliveryWarningFlag,deliveryNotifFlag) match {
                           case (true,false) => "1"
                           case (false,true) => "2"
                           case (true,true) => "3"
                           case _ => ""
                         }

   val data = lineText(t.posDataBlock).replace("\\r\\n","\r\n")
 
   val bloc1 = "{1:F01" + sender + repeatChar('0',9) + "}"
   val bloc2 = "{2:" + sens + mt + receiver + priority + "}"

   val bloc3 = (field108,field119) match {
                 case (None,None) => ""
                 case _ => "{3:" + field108.getOrElse("") + field119.getOrElse("") + "}"
                }

   val bloc4 = "{4:" + data + "\r\n-}"  

   print("Umid["+lt._1+"] done\n")

   bloc1 + bloc2 + bloc3 + bloc4   
  
  }

}

val config:ConfigProperties = getConfigProperties match {
	case Some(s) => s
        case _ => sys.error("Config not found")
}

def main(args:Array[String]) = {

    
    val senderBic = config.getParameter("SND_ADDR")  
    val receiverBic = config.getParameter("RCV_ADDR") 
    val folderArchive = config.getParameter("ARCHIVE_DIR")
    val umidsParam = config.getParameter("UMIDS")
    val rjeFileName = "out.rje"

    val umids = umidsParam.replace("\"","").replace("'","").split(",").toList

    val itMesg = generateMesgFromArchive(umids,folderArchive);
           
    val fileWriterRes = new FileWriter(rjeFileName);
    fileWriterRes.write(itMesg.toList.mkString("$"));
    fileWriterRes.close();

}

}
