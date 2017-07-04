import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import java.io.FileWriter


class Template () {
 var umid = "";
 var syntaxTable  = "" ;
 var creaOperNickName = "" ;
 var modOperNickName= "";
 var sender = "" ;
 var name ="";
 var unitName= "";

 override def toString = name + ";" + syntaxTable + ";" + unitName + ";" + sender + ";" + creaOperNickName + ";" + modOperNickName

 def toCsv = name + ";" + syntaxTable + ";" + unitName + ";" + sender.take(8) + ";" + creaOperNickName + ";" + modOperNickName

}

case class NewTemplate(umid:String)
case class NewField(field:String, value:String)
case object GetTemplates

class TemplatesFile extends Actor {

private var templates:List[Template] = Nil;
private var currentTemplate = new Template

def act = loop {
 react {
  case NewTemplate(umid) =>  {
		     if (currentTemplate.umid != "") templates = currentTemplate :: templates
		     currentTemplate = new Template
                     currentTemplate.umid = umid
		    }
                   
 
  case NewField(field,value) => field match {
                                  case "mesg_syntax_table_ver" => currentTemplate.syntaxTable = value;
                                  case "mesg_sender_X1" => currentTemplate.sender = value;
                                  case "mesg_template_name" => currentTemplate.name = value;
				  case "mesg_crea_oper_nickname" => currentTemplate.creaOperNickName = value;
                                  case "mesg_mod_oper_nickname" =>  currentTemplate.modOperNickName = value;
                                  case "inst_unit_name" =>  currentTemplate.unitName = value;

                                  case _ => {}
                                 }

  case GetTemplates => reply(templates)

  case _ => {}
} 
}
}




object Templates extends RegexParsers {

override def skipWhitespace = false

val templatesFile = new TemplatesFile()
templatesFile.start

val anyValue   = regex(new Regex(".+"))
val idField = "[a-zA-Z0-9_]+".r
val number= "[0-9]+".r  

val mesgUmid = "mesg_s_umid," ~> ((number ~ ",") ~> anyValue) ^^ ( s => templatesFile ! NewTemplate(s) ) 
val otherField = (idField <~ ",") ~ ((number ~ ",") ~> anyValue) ^^ { case n ~ s => templatesFile ! NewField(n,s) }

val field = (mesgUmid | otherField ) 

def parseField(line: String) = parse(field,line)

def load = {

 val itTemplates = scala.io.Source.fromFile(new File("C:/USERS/templates/templatesv6.txt"),"ISO-8859-1").getLines
 itTemplates.foreach( s => parseField(s))

 templatesFile !? GetTemplates match {
  case t: List[Template] => t
  case _ => Nil
}
  
}


}
val templates = Templates.load
val oldTemplates = templates.filter( t => t.syntaxTable!="1105")

