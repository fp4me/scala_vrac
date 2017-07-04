import scala.xml._
import java.text.SimpleDateFormat
import java.util.Properties
import java.io.FileInputStream
import java.io.FileWriter
import java.util.Date

object OperUsers {


class  UARProperties() {

  private val props:java.util.Properties = new java.util.Properties()
  props.load(new FileInputStream("saaOpers.properties"))
  def getParameter(key: String): String = Option(props.getProperty(key)) match {
			case Some(value) => value
			case None => "" }
}

def getUARProperties : Option[UARProperties] = {
try { Some(new UARProperties) } catch { 
 case _ =>  Console.println("config file[saaOpers.properties] not found");
            None
            
 }
}

def getOpersXmlContent(conf:UARProperties) : Option[scala.xml.Elem] = {
val pathOpersFile = conf.getParameter("PATH_OPERS_FILE");
try {     
     val srcxml = scala.io.Source.fromFile(pathOpersFile).mkString 
     Some(XML.loadString(srcxml))
    } 
catch  {
     	case _ => 	Console.println("Error on loading xml file[" + pathOpersFile + "]");
			None
       }
}

def getConfigAndData : Option[(UARProperties,scala.xml.Elem)] = {
 getUARProperties match 
  {
   case Some(config) => getOpersXmlContent(config) match
    {
     case Some(xmlData) => Some((config,xmlData))
     case _ => None
    } 
   case _ => None
}
}
							 
def FirstandLastName(d:String) : String = d.dropWhile(_ != ' ').trim + ";" +  d.takeWhile(_ != ' ') 

val formatDatefromExtract = new SimpleDateFormat("dd/MM/yy HH:mm:ss")
val formatDateUar = new SimpleDateFormat("yyyy-MM-dd")
val formatDateResFile = new  SimpleDateFormat("yyyyMMdd")

def formatDate(di:String) : String = di.length match { 
  case 0 => ""
  case _ => try { 
                 formatDateUar.format(formatDatefromExtract.parse(di))
                } catch {
                case _ => ""
                }
}

def main(args:Array[String]) = {

 getConfigAndData match {

 case Some((config,opersxml)) => {

		   val applCode = config.getParameter("APPL_CODE");
                   val subArea = config.getParameter("SUB_AREA");
		   val dirRes = config.getParameter("DIR_RES");
		   val isRTFE = config.getParameter("RTFE")=="Y";
		   val dept = "";
                   val country = "";
                   val comment = "";
                   val passwdEx= "";
                   val userIgg= "";
		   val seqOperatorDefn = (opersxml \\ "OperatorDefn") 


		   val contentUAR = seqOperatorDefn.map (entry =>
                     {   	   
		      applCode + ";" +
                      subArea + ";" +
		      FirstandLastName((entry \ "Operator" \ "Description").text) + ";"  +
                      (entry \ "Operator" \ "Identifier" \ "Name").text + ";" + 
		      dept + ";" +
		      country + ";" +
                      formatDate ((entry \ "LastSignOn").text) + ";" +
		      
                      (if ((entry \ "Operator" \ "AuthenticationType").exists(x => x.text="LDAP")) "RTFE" else formatDate ((entry \ "LastChanged").text))  + ";" +
		      (entry \ "Operator" \ "Profile").foldLeft("")((x,y) => x.length match { case 0 => y.text; case _ => x + "|" + y.text}) +
		      "@@"+ 
		      (entry \ "Operator" \ "Unit").foldLeft("")((x,y) => x.length match { case 0 => y.text; case _ => x + "|" + y.text}) + ";" +
                      comment + ";" +
		      passwdEx + ";" +
		      userIgg + ";"
		     })

		   val operResFileName = "UAR_" + applCode + "-" + subArea + "_" + formatDateResFile.format(new Date()) + "_In.csv";
		   
                   
                   try {
                        val fileWriterRes = new FileWriter(operResFileName);
		        fileWriterRes.write(contentUAR.mkString("\n"));
		        fileWriterRes.close();
			Console.println("\n" + seqOperatorDefn.length + " operators exported succesfully in UAR ["+ operResFileName + "]"); 
		       } catch { case _ => Console.println("Error on writing UAR in ["+ operResFileName + "]");
			                   System.exit(2);
			       }		   

}
 case None => System.exit(1);
}
}
}

