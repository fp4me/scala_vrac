import scala.util.matching.Regex
import scala.util.parsing.combinator._
import java.util.Date
import java.text.SimpleDateFormat
import java.io.FileWriter

object Test extends RegexParsers {

val formatDateFromExtract = new SimpleDateFormat("dd/MM/yy HH:mm:ss")

def loadFile = {
 val lines:List[String] = Nil 
 val myIterator = scala.io.Source.fromFile("C:/USERS/interv.txt").getLines
 (myIterator.foldLeft(lines,"")(parcours)._1) 
}

def getDate(d:String):Date = { formatDateFromExtract.parse(d)}

val DAY_MILLIS = 1000 * 60 * 60 * 24;

def parcours(a:(List[String],String),b:String):(List[String],String) = {
  if ( a._2.length==0) ((b :: a._1),b) 
  val lastfields = a._2.split('|'); 
  val curfields = b.split('|');
  if ((lastfields(0) == curfields(0))&&(lastfields(1) == curfields(1))) {
    val lastdate = getDate(lastfields(2)) ;
    val curdate = getDate(curfields(2)) ;
    val difInDays =  (curdate.getTime() - lastdate.getTime) / DAY_MILLIS  ;
    if (difInDays > 2) (((a._2+"|"+difInDays) :: a._1),b)  else (a._1,b)  
   }
  else (a._1,b) 
}

def writeRes= {
val file = new FileWriter("resultat.out")
val all = Test.loadFile
all.map(l => file.write(l+"\n"))
file.close
}
}
