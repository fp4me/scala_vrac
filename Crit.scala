import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor


case class Criteria (keyw:String,oper:String,value:String) {
 override def toString ="keyword[" ++ keyw ++ "] operator[" ++ oper ++ "] value[" ++ value ++ "]"
}


object Criterias extends RegexParsers {
 
 override def skipWhitespace = false

 lazy val anyValue   = regex(new Regex(".+"))

 val lineCriteria  = "rule_criteria =" ~ (elem(' ')).*  ~> anyValue
 val lineEndCriteria = "rule_internal_criteria = " <~ anyValue 


 def parseCriteria(line: String) = parse(lineCriteria, line)
 def parseEndCriteria(line: String) = parse(lineEndCriteria, line)

 
 def parcours(a:(List[String],String),b:String):(List[String],String) = { 
	if (a._2.isEmpty) 
         {
          if (parseCriteria(b).successful) (a._1,parseCriteria(b).get) else (a._1,a._2)
	 }
        else 
	 {
          if ((parseCriteria(b).successful)||(parseEndCriteria(b).successful)) ((a._2 :: a._1), "") else (a._1,a._2 + b)
         }  
}

 def loadCriteriasV6 = {
   
   var criterias:List[String] = Nil

   val myIterator = scala.io.Source.fromFile("C:/USERS/configParis.out").getLines
   myIterator.foldLeft(criterias,"")(parcours)
   
 }
    
}


