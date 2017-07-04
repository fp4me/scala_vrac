import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor


case class Instance(typ:String,num:Int) 


class Enr(umid:String) {
 def getUmid = umid
}

class Intv(umid:String,instNum:Int,seqNbr:Int,name:String,text:String) extends Enr(umid) {
  def getInstNum = instNum 
  def getSeqbr = seqNbr
  def getName = name 
  def getText = text
  override def toString = "umid:"+umid + " instNum:" + instNum + " seqNbr:" + seqNbr + " name:" + name + " text: "+ text 
}

class Mesg(umid:String, creaDateTime:String, receiver:String, sender:String, sens:String, trn:String, mt:String) {
  def getUmid = umid
  def getCreaDateTime = creaDateTime
  def getReceiver = receiver
  def getSender = sender
  def getSens = sens
  def getTrn = trn
  def getMt = mt
}

class Inst(umid:String,num:Int,appeSeqNbr:Int) extends Enr(umid) {
  def getNum = num 
  def getAppeSeqNbr = appeSeqNbr
  override def toString = "umid:"+umid + " num:" + num + " appeSeqNbr:" + appeSeqNbr 
}

class Appe(umid:String,instNum:Int,seqNbr:Int,creaRpName:String) extends Enr(umid) {
   def getInstNum = instNum
   def getSeqNbr = seqNbr
   def getCreaRpName = creaRpName
  override def toString = "umid:"+umid + " instNum:" + instNum +  " seqNbr:" + seqNbr +" creaRpName:" + creaRpName 
}

case class RuleMsg(rule:(Int,String,Int))

class MsgActor extends Actor {
var rules:List[(Int,String,Int)] = Nil;

def act = loop {
  react {
    case RuleMsg(rm) => rules = rm :: rules
    case getRules => reply (rules.reverse)
   } 
}
}

object History extends RegexParsers {
 
 override def skipWhitespace = false

 lazy val space = (elem(' ') | elem('\t') | elem('\r')).+
 lazy val value   = regex(new Regex("[a-zA-Z0-9-_%/.!:]+"))
 lazy val valueWithSpace   = regex(new Regex("[a-zA-Z0-9-_%/.!: ]+"))
 lazy val keyword = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val number = regex(new Regex("[0-9]+")) ^^ (n => n.toInt)
 lazy val valueWithQuotes = "'" ~> valueWithSpace <~ "'"
 lazy val amount = regex(new Regex("[0-9.,]+"))
 lazy val operator = ">=" | ">" | "<=" | "<" | "!=" | "=" | "like" 


 lazy val multivalueWithQuotes = valueWithQuotes ~ (rep((" " ~> ("or"|"and")) ~ (" " ~> valueWithQuotes) ^^ { case (fst:String) ~ (valu:String) => (" " + fst + " " + valu) } )) ^^ {
       case (first:String) ~ (second:List[String]) => (first::second).mkString("")
		}

 lazy val funcValue = "amount" ~ opt(space) ~ "(" ~ opt(space) ~ "'" ~> (amount <~ "'" ~ opt(space) ~ ")") ^^ { case (amnt:String) => "amount ("+amnt+")" }     

 lazy val critWithoutPar = (keyword <~ opt(space)) ~ operator ~ (opt(space) ~> (multivalueWithQuotes | funcValue | value)) ^^ {
                case (keyw:String) ~ (oper:String) ~ (vali:String) => "yes" }

 lazy val critWithPar = "(" ~> critWithoutPar <~ ")"
 lazy val crit = (critWithoutPar | critWithPar ) 

 lazy val elemExpr = (opt("not" <~ opt(space)) ~ exprWithPar | opt("not" <~ space) ~ crit)  ^^ { case (first:Option[String]) ~ (second:String) => if (first.isEmpty) second else "not(" + second + ")" }

 lazy val exprWithPar: Parser[String] = "(" ~ opt(space) ~> expr <~ opt(space) ~ ")" 

 lazy val exprWithoutPar: Parser[String] =  elemExpr ~ rep(((opt(space) ~> ("and" | "or")) ~ (exprWithPar|(space ~> elemExpr))) ^^ { case (condi:String) ~ (ii:String) => condi + "(" + ii + ")" } ) ^^ {
                     case (first:String) ~ (second:List[String]) => (first::second).mkString("\n")
		}

 lazy val expr  = (exprWithoutPar | exprWithPar) <~ opt(space)
 

 def filterCriteria(crit:String) = crit.filter((c:Char) => (c!='\n' && c!=10) )

 def parseItem(str: String) :Boolean = parse(expr, filterCriteria(str)) match  {
 				case s @ Success(_,in) => if (in.atEnd) true else false 
				case _ => false			
	} 

 def parseI(str: String) = parse(expr, filterCriteria(str))

 val anyValue   = regex(new Regex(".+"))

 
 val lineCriteria  = "rule_criteria =" ~ (elem(' ')).*  ~> anyValue
 val lineEndCriteria = "rule_internal_criteria = " <~ anyValue 

 val lineRuleIntvText = "rule_rule_intv_text [0] = "  ~> anyValue
 val lineEndRuleIntvText = "rule_rule_intv_text [1] = "  ~> opt(anyValue)
 
 def parseNewIntv(line:String) = parse(lineRuleIntvText,line)
 def parseEndNewIntv(line:String) = parse(lineEndRuleIntvText,line)

 def parseCriteria(line: String) = parse(lineCriteria, line)
 def parseEndCriteria(line: String) = parse(lineEndCriteria, line) 
 
 lazy val idOperator = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val rpName = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val resultLabel = regex(new Regex("[a-zA-Z0-9_ ]+"))
 lazy val funcName = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val unitName = regex(new Regex("[a-zA-Z0-9_]+"))

 val routedFromAction = ("Routed from rp [" ~>  rpName ) ~ ( "] to rp [" ~> (rpName <~ "]; " ))  ~ opt( "1 instance(s) created at [" ~> ( rpName <~ "] respectively;")) ~ ( "On Processing by Function " ~> funcName ) ~ (" with result " ~> opt(resultLabel)) ~ (";(Rule:" ~> ("USER"|"SYSTEM")) ~ ("," ~> number) <~ ")"    

 val completedInAction = ("Completed in rp [" ~>  rpName ) ~ ( "]; On Processing by Function " ~> funcName ) ~ (" with result " ~> opt(resultLabel)) ~ (";(Rule:" ~> ("USER"|"SYSTEM")) ~ ("," ~> number) <~ ")"    
 
 val createdAction = ("Created at rp [" ~>  rpName)  ~  ("] and assigned to unit [" ~> unitName) <~ "]"

 val lineActionBy = (("By " ~> idOperator) <~ " : ") ~ (createdAction | routedFromAction | completedInAction)

 def parseFile(i:Iterator[String]) = { 

 val instanceOriginal = "Original"  ^^  (s => new Instance("Original",0)) 
 val instanceNew = ("Copy"|"Notification") ~ (" - " ~> number) ^^ { case (typ:String) ~ (num:Int) => new Instance(typ,num) }
 val lineInstance = "*" ~> ((instanceOriginal|instanceNew) <~ " (" ~ anyValue)      

 
 def parseInstances(line : String) = parse (lineInstance,line); 

 def itFile(a:(List[Instance],String),b:String):(List[Instance],String) = { 
 var res:(List[Instance],String) =(Nil,"");
 if (a._2.isEmpty) 
  {
   if (parseInstances(b).successful) res = (parseInstances(b).get :: a._1, a._2) else res = a
  }
 res
}

var instances:List[Instance] = Nil
(i.foldLeft(instances,"")(itFile))._1.reverse
}

def parseIntv2(i:Iterator[String]) = {
 var isPrint=0;
 i.map(li => if (li.split(",").length<18) {if ((isPrint<2)) {print (li+"\n");isPrint += 1 };""} else 
         {if ((li.split(",")(7)!="\"INTY_OTHER\"")&&(li.split(",")(7)!="\"INTY_DELIVERY_REPORT\"")) li.split(",")(17).filter(c => c!='"').take(15) else ""})
}


def exploreMsg(umid:String) = {
  val intvs = checkIntv
  val intvsMsg = intvs.getOrElse(umid,Nil)
  val actMsg = new MsgActor
  val routes = intvsMsg.filter(_.getName=="Instance routed")
 

 
  def parseRouteIntv(text:String) = {
                                     val resParse = parse(routedFromAction,text).get
				     resParse match { 
                                     case ((rpFrom:String) ~ (rpTo:String) ~ (instCreated:Option[String]) ~ (funcName:String) ~ (res:Option[String]) ~ (ruleType:String) ~ (ruleNumber:Int)) => rpFrom + ":" + ruleNumber + "\t\t\t new Inst:" + instCreated.getOrElse("None");
				     case _ => "Error"
				    }
}
 
 routes foreach (x => println (parseRouteIntv(x.getText))) 
}
  

def checkInstances = {
  val insts = parseFile(scala.io.Source.fromFile("C:/USERS/mt543.hist").getLines)  
  insts
}

def checkIntv2 = {
 val intvs = parseIntv(scala.io.Source.fromFile("C:/USERS/archiveV6/intvEURASIA.txt").getLines)
 intvs
}

def parseIntv(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if (champs.length>17) if (champs(7) !="\"INTY_OTHER\"" && champs(7)!="\"INTY_DELIVERY_REPORT\"")  }
    yield new Intv(champs(1),champs(2).toInt,champs(4).toInt,champs(6),champs(17))
}


def checkIntv = {
 val intvs = parseIntv(scala.io.Source.fromFile("C:/USERS/archiveV6/analyse/intv111222.txt").getLines.drop(1))
  toMapByUmid(intvs)
}

def parseMesg(i:Iterator[String]) = {
 for {li <- i ; val champs =   li.split("\",").map(_.drop(1)) if champs.length>73 }
               yield new Mesg(champs(1),champs(15),champs(46),champs(65),champs(69),champs(72),champs(73))
}


def checkMesg = {
 val mesgs = parseMesg(scala.io.Source.fromFile("C:/USERS/archiveV6/analyse/mesg111222.txt").getLines.drop(1))
 mesgs.map(me => (me.getUmid,me)).toMap 
}

def parseInst(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if champs.length>9 }
               yield new Inst(champs(1),champs(2).toInt,champs(8).toInt)
}

def checkInst = {
 var instsMap = Map[String,List[Inst]]()
 val insts = parseInst(scala.io.Source.fromFile("C:/USERS/archiveV6/analyse/inst111222.txt").getLines.drop(1))
 toMapByUmid(insts)
}

def toMapByUmid[A <: Enr](i:Iterator[A]) = {
  var instsMap = Map[String,List[A]]()
  val res = i.foldRight(("",List[A]()))((a,b) => if (a.getUmid!=b._1) {if (b._1!="") instsMap += (b._1 -> b._2) ; (a.getUmid,List[A](a)) } else (b._1,a :: b._2))
  if (res._1!="") instsMap + (res._1 -> res._2) else instsMap  
  
}

def parseAppe(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if champs.length>14 }
               yield new Appe(champs(1),champs(2).toInt,champs(4).toInt,champs(13))
}

def checkAppe = {
 val appes = parseAppe(scala.io.Source.fromFile("C:/USERS/archiveV6/analyse/appe111222.txt").getLines.drop(1))
 toMapByUmid(appes)
}



}
