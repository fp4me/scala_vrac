import java.io.File

//import scala.collection.JavaConversions._ 

def search(folder:String,ext:String):Array[(String,Long)] = {
 
 val dir = new File(folder)
 val contentDir = new File(folder) listFiles  match {
                                   case null => new Array[java.io.File](0)
				   case files => files
                                   }
                   
 val listeDir = contentDir.filter(_.isDirectory)
 val resDirs = listeDir.flatMap(x => search(x.getPath,ext));
 val listeRes = contentDir.filter(file => file.getName.endsWith("."+ext)).map(x => (x.getPath,x.length)) ++ resDirs
 listeRes.sortWith(_._2>_._2)

}


abstract class Hex(val num1: Int){} //These are declared in their own files
abstract class Side {val sideString = "This is a side"} 
trait DescripTypes {  
  type HexT <: Hex 
  type SideT <: Side 
} 

def good = { println("good") }

object Fields {

var keepField:PartialFunction[String,Unit] = { case "" => print("error"); }

def add(field:String,code: => Unit) = keepField.isDefinedAt(field) match {
                                                       case false => keepField =  keepField orElse { case f if f == field => code }
 						       case _ => ()
						 }

def forceAdd(field:String,code: => Unit) = keepField =   ({ case f if f == field => code }:PartialFunction[String,Unit]) orElse keepField
 						      

def apply(field:String) = keepField.lift(field) match {
                             case None => ()
                             case Some(io) => io
                            }
}

