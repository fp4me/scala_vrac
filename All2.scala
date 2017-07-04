
import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor
import java.text.SimpleDateFormat
import java.util.Date
import java.io.FileInputStream
import java.util.Properties


object Config {

class ConfigTool private (
        val cfgSaa:String,
	val mesgFile:String,
	val appeFile:String,
	val intvFile:String,
	val instFile:String) {

def getCfgSaaFile = cfgSaa
def getMesgFile = mesgFile
def getAppeFile = appeFile
def getIntvFile = intvFile
def getInstFile = instFile

}


object ConfigTool {

class  ToolProperties() {

  private val props:java.util.Properties = new java.util.Properties()
  props.load(new FileInputStream("tool.properties"))

  def getParameter(key: String): String = Option(props.getProperty(key)) match {
			case Some(value) => value
			case None => "" }
}

def apply():Option[ConfigTool] = {
 
 try { val toolProperties = new ToolProperties;
       val configTool = new ConfigTool(toolProperties.getParameter("CFG_SAA_FILE"),
			               toolProperties.getParameter("APPE_FILE"),
			               toolProperties.getParameter("APPE_FILE"),
				       toolProperties.getParameter("INTV_FILE"), 
				       toolProperties.getParameter("INST_FILE"));
       Some(configTool)
} 
 catch { 
 	case _ =>  Console.println("config file[tool.properties] not found");
        None            
       }
}
}

}


case class Criteria (keyw:String,oper:String,value:String) {
 override def toString ="keyword[" ++ keyw ++ "] operator[" ++ oper ++ "] value[" ++ value ++ "]"
}


class RoutingPoint(name:String,assignFunc:String) {
 override def toString = "Name:" + name + " ["+ assignFunc+ "]"
 def getAssignFunc = assignFunc
 def getName = name
}

case class UpdateRP(s:String)
case class UpdateAssignFunc(s:String)

case class SchemaName(s:String)
case object SchemaActive
case object GetSchemaActive


object ConfigActor extends Actor {

object FuncResLabel  {


val funcResLabelDefault:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" )


val funcResLabelDummyMpfn:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" )


val funcResLabelSiToSWIFT:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "MAC No Keys" , 
                                           "V2" -> "Nacked" ,
                                           "V3" -> "Mac Authentication Error" ,
                                           "V4" -> "PAC No Keys" ,
                                           "V5" -> "PAC Authentication Error" ,
                                           "V6" -> "Inactive correspondent" ,
                                           "V12" -> "Not authorised by RMA" ,
                                           "V13" -> "Authorisation not present" ,
                                           "V18" -> "Authorisation/Bilateral key not present" )

val funcResLabelSiToSWIFTNet:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "No authorisation" , 
                                           "V2" -> "Authorisation not enabled" ,
                                           "V3" -> "Authorisation not in validity period" ,
                                           "V4" -> "Authorisation does not allow message" )

val funcResLabelSiFromSWIFTNet:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "No authorisation" , 
                                           "V2" -> "Authorisation not enabled" ,
                                           "V3" -> "Authorisation not in validity period" ,
                                           "V4" -> "Authorisation does not allow message" ,
                                           "V5" -> "Signature verification on failure" )

val funcResLabelFofa:Map[String,String] = Map("V1" -> "OFAC Decision Passed" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V2" -> "OFAC Decision Failed" ,
                                           "V3" -> "OFAC Decision re-Check" ,
                                           "V4" -> "OFAC Decision Pending" ,
                                           "V5" -> "OFAC No Review" ,
                                           "V6" -> "OFAC Extracted" ,
                                           "V7" -> "OFAC Decision Cancelled")

val funcResLabelFofs:Map[String,String] = Map("V1" -> "OFAC test passed" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V2" -> "OFAC test failed" ,
                                           "V3" -> "OFAC agent error" ,
                                           "V4" -> "OFAC agent bypassed" ,
                                           "V5" -> "OFAC test nonblocking" ,
                                           "V6" -> "FML not filtered" )

val funcResLabelSiFromSWIFT:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "MAC No Keys" , 
                                           "V2" -> "MAC Future Key" ,
                                           "V3" -> "MAC Previous Key" ,
                                           "V4" -> "PAC No Keys" ,
                                           "V5" -> "PAC Future Key" ,
                                           "V6" -> "PAC Previous Key" ,
                                           "V7" -> "FIN-Copy service bypassed" ,
                                           "V8" -> "MAC Authentication Error" ,
                                           "V9" -> "PAC Authentication Error" ,
                                           "V10" -> "097 PAC Authentication Error" ,
                                           "V11" -> "097 PAC No Keys" ,
                                           "V12" -> "Not Authorised by RMA" ,
                                           "V13" -> "Authorisation not present" ,
                                           "V14" -> "Signature Auth. failure" ,
                                           "V15" -> "Invalid Sign DN" ,
                                           "V16" -> "Invalid digest" ,
                                           "V17" -> "Invalid Certificate Policy ID" ,
                                           "V18" -> "Authorisation/Bilatery key not present" )


val funcResLabelUsea:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
					   "V1" -> "No Keys" , 
                                           "V2" -> "Future Key" ,
                                           "V3" -> "Previous Key" ,
                                           "V4" -> "Discard" ,
                                           "V5" -> "Bypass" ,
                                           "V6" -> "Recoverable error" ,
                                           "V7" -> "Unrecoverable error" ,
                                           "V8" -> "Cancel" )

val funcResLabelUseaAn:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V6" -> "Recoverable error" , 
                                           "V7" -> "Unrecoverable error" ,
                                           "V9" -> "Message for automatic processing" ,
                                           "V11" -> "Message for semi-automatic processing" )

val funcResLabelAiFromAppli:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" ,
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "Disposition Error" , 
                                           "V2" -> "Original broadcast" )

val funcResLabelMpmOrMpc:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" ,
					   "WILDCARD" -> "Wildcard" ,
					   "V1" -> "Discard" , 
                                           "V2" -> "Invalid message" )
 
val funcResLabelTisFrm:Map[String,String] =  Map("SUCCESS" -> "Success",
					   "FAILURE" -> "Failure" ,
					   "WILDCARD" -> "Wildcard" ,
			                   "V1" -> "Teskey failure" , 
                                           "V2" -> "Format failure" ,
                                           "V3" -> "Address failure" )

val funcResLabelTisInOut:Map[String,String] =  Map("SUCCESS" -> "Success",
					    "FAILURE" -> "Transmission Error" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "Delivery Report" ,
                                           "V2" -> "Nacked" ,
                                           "V3" -> "Testkey failure", 
                                           "V4" -> "Inactive correspondent", 
                                           "V5" -> "Awaiting Delivery")

val funcResLabelTrRec:Map[String,String] =  Map("SUCCESS" -> "Delivered" , 
					   "WILDCARD" -> "Wildcard" ,
                                           "V1" -> "Not matched" ,
                                           "V2" -> "Not delivered" ,
                                           "V3" -> "Delayed delivery")

val funcResLabelSMQSFromMqSeries:Map[String,String] =  Map("SUCCESS" -> "Accept",
					   "FAILURE" -> "Reject" , 
					   "WILDCARD" -> "Wildcard" ,
					   "V1" -> "Validation Error" , 
                                           "V2" -> "Nacked" ,
                                           "V3" -> "MQ Error" ,
                                           "V4" -> "Recovery")

val funcResLabelSMQSToMqSeries:Map[String,String] =  Map("SUCCESS" -> "Accept",
					   "FAILURE" -> "Reject" , 
					   "WILDCARD" -> "Wildcard" ,
					   "V1" -> "Validation Error" , 
                                           "V2" -> "MQ Error" ,
                                           "V3" -> "Code Error")

val labels:Map[String,Map[String,String]] = Map("_SI_system_msg" -> funcResLabelDefault,
                                                "Dummy_mpfn" -> funcResLabelDefault,
                                                "_SS_alarm_creation" -> funcResLabelDefault,
                                                "_SI_delivery_subset" -> funcResLabelDefault,
                                                "_SI_to_SWIFT" -> funcResLabelSiToSWIFT,
                                                "_SI_from_SWIFT" -> funcResLabelSiFromSWIFT,
                                                "SS_usea" -> funcResLabelUsea,
                                                "SS_usea_an" -> funcResLabelUseaAn,
                                                "AI_from_APPLI" -> funcResLabelAiFromAppli,
                                                "AI_to_APPLI" -> funcResLabelDefault,
                                                "mpa" -> funcResLabelDefault,
                                                "mpm" -> funcResLabelMpmOrMpc,
                                                "mpc" -> funcResLabelMpmOrMpc,
                                                "TIS_frm" -> funcResLabelTisFrm,
                                                "TIS_in_out" -> funcResLabelTisInOut,
                                                "TR_REC" -> funcResLabelTrRec,
                                                "SMQS_From_MQSeries" -> funcResLabelSMQSFromMqSeries,
                                                "SMQS_To_MQSeries" -> funcResLabelSMQSToMqSeries,
                                                "FOFA_mpf" -> funcResLabelFofa,
                                                "FOFS_mpf" -> funcResLabelFofs,
                                                "rma" -> funcResLabelDefault,
                                                "RMS_flow" -> funcResLabelDefault,
                                                "_SI_to_SWIFTNet" -> funcResLabelSiToSWIFTNet,
                                                "_SI_from_SWIFTNet" -> funcResLabelSiFromSWIFTNet)


}


private var rps = Map.empty[String,RoutingPoint]
private var currentRpName = "";
private var currentSchema = "" 
private var schemaActive = "" 

def act = loop {
   react {
    	  case SchemaName(sn) => currentSchema = sn;
          case SchemaActive => schemaActive = currentSchema; 
          case GetSchemaActive => reply (schemaActive);
          case UpdateRP(s) => currentRpName = s;
          case UpdateAssignFunc(s) => {rps += (currentRpName -> new RoutingPoint(currentRpName,s))}
}
}
                           
def getLabelFuncRes(rpName:String,funcRes:String):String = rps.get(rpName) match {
								case None => funcRes
								case Some(rp) => FuncResLabel.labels.get(rp.getAssignFunc) match {
									      case None => funcRes
									      case Some(labels) => labels.get(funcRes) match {
											case None => funcRes
											case Some(label) => label
											}
									 }
}

def getAssignFunc(rpName:String):String = rps.get(rpName) match {
								case None => "None"
								case Some(rp) => rp.getAssignFunc
                                                               }
}


class Rule () {
 var rpName:String="";
 var rtName:String="";
 var ruleSeqNbr:Int=(-1);
 var ruleSchemas:String="";
 var ruleDesc:String="";
 var criterias:List[Criteria] = Nil;
 var condition:String="";
 var ruleSourceAction = "";
 var ruleNewAction = "";
 var ruleSourceTargetRpName= "";
 var ruleNewTargetRpName= "";
 var ruleNewActionType= "";
 var ruleNewUnitName= "";
 var ruleNewIntv = "";
 var funcRes = Map.empty[Int,String]

 override def toString ="[" ++ rtName ++ "][" ++ rpName ++ ":" ++ ruleSeqNbr.toString ++ "] schemas[" ++ ruleSchemas ++ "]" ++ 
                        "\nDesc[" ++ ruleDesc ++ "]" ++
			printFuncRes ++ 
			"\nCondition:\n" ++ condition ++
			"\n" + printRouting  

  def printRouting = "source -> :" + (if (ruleSourceTargetRpName.isEmpty) "" else ruleSourceTargetRpName) + "\n" +
                     "new -> :" + (if (ruleNewTargetRpName.isEmpty) "" else ruleNewTargetRpName) + "\n"
   

 def printFuncRes =  if (funcRes.isEmpty) "" else 
                     ("\nFunction Result Selected:" + funcRes.values.foldLeft(""){ (s,e) => s + "\n -> " + ConfigActor.getLabelFuncRes(rpName,e) } ++ "\n")

}

case object ShowCriteria 
case class add(c:Criteria)
case class addMap(c:Criteria)


case class RpName(rpn:String)
case class RtName(rtn:String) 
case class RuleSeqNbr(rsn:String)
case class RuleSchemas(rs:String)
case class RuleDesc(rd:String)
case class RuleCond(rc:(String,List[Criteria]))
case class RuleMpfnResult(fr:(Int,String))
case class RuleSourceAction(s:String)
case class RuleNewAction(s:String)
case class RuleTargetRpName(s:String)
case class RuleNewTargetRpName(s:String)
case class RuleNewActionType(s:String)
case class RuleNewUnitName(s:String)
case class RuleNewIntv(s:String)
case object GetRules
case object AddRule

case object GetCriterias 
case object InitCriterias 


class RulesActor extends Actor {
private var rules:List[Rule] = Nil;
private var currentRule = new Rule

def act = loop {
   react {
    case RpName(rpn) => currentRule.rpName = rpn;
    case RtName(rtn) => currentRule.rtName = rtn;
    case RuleSeqNbr(rsn) => currentRule.ruleSeqNbr  = rsn.toInt;
    case RuleSchemas(rs) => currentRule.ruleSchemas = rs;
    case RuleDesc(rd) => currentRule.ruleDesc = rd;
    case RuleSourceAction(ac) => currentRule.ruleSourceAction = ac;
    case RuleNewAction(ac) => currentRule.ruleNewAction = ac;
    case RuleTargetRpName(rp) => currentRule.ruleSourceTargetRpName = rp;
    //case RuleNewTargetRpName(rp) => if (currentRule.ruleNewAction!="INST_TYPE_NONE") currentRule.ruleNewTargetRpName = rp;
    case RuleNewTargetRpName(rp) => currentRule.ruleNewTargetRpName = rp;
    case RuleNewActionType(at) => currentRule.ruleNewActionType = at;
    case RuleNewUnitName(un) => currentRule.ruleNewUnitName = un;
    case RuleNewIntv(intv) => currentRule.ruleNewIntv = intv;
    case RuleCond(rc) => {
                          currentRule.condition = rc._1;
                          currentRule.criterias = rc._2;
			  }
    case GetRules => reply (rules);
    case AddRule => {
		     rules = currentRule :: rules
		     currentRule = new Rule
		    }
    case RuleMpfnResult(fr) => {
 				currentRule.funcRes += (fr._1 -> fr._2)                               
                                if (ConfigActor.getLabelFuncRes(currentRule.rpName,fr._2)==fr._2) println(ConfigActor.getAssignFunc(currentRule.rpName)+" "+fr._1+":"+ConfigActor.getLabelFuncRes(currentRule.rpName,fr._2))
                               }
}
}
}




class MyActor extends Actor {
 var infos: List[String] = Nil
 var criterias:List[Criteria] = Nil
 var mymap = Map.empty[String,List[String]]  
 def act = loop {
   react {
     
     case s:String => { infos = s :: infos ; Console.println(s); }
     case add(c) => { criterias = c :: criterias }
     case addMap(c) => { mymap.get(c.keyw) match {
                           case None => mymap = mymap + (c.keyw -> List(c.value)) 
			   case Some(l) => mymap = mymap.updated(c.keyw,(c.value :: l )) }
			}
   
     case ShowCriteria => println (criterias.mkString("\n"));
     case InitCriterias => { criterias = List[Criteria]() }
     case GetCriterias => reply (criterias)
  }
 }

 def showCriterias = mymap.toList.map(ff).mkString("\n");
 def ff(param:(String,List[String]) ) : String = param._2.distinct.map(s2 => (param._1 ++ ";" ++ s2)).mkString("\n");

 
}



object Criterias extends RegexParsers {
 
 override def skipWhitespace = false

 val configActor = ConfigActor
 configActor.start

 val myA = new MyActor()
 myA.start
 lazy val space = (elem(' ') | elem('\t') | elem('\r')).+
 lazy val value   = regex(new Regex("[a-zA-Z0-9-_%/.!:]+"))
 lazy val valueWithSpace   = regex(new Regex("[a-zA-Z0-9-_%/.!: ]+"))
 lazy val keyword = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val number = regex(new Regex("[0-9]+")) ^^ (n => n.toInt)
 lazy val valueWithQuotes = "'" ~> valueWithSpace <~ "'"
 lazy val amount = regex(new Regex("[0-9.,]+"))
 lazy val letter = regex(new Regex("[A-Z]"))
 lazy val rpname = keyword
 lazy val espace = (elem(' ')).+
 lazy val operator = ">=" | ">" | "<=" | "<" | "!=" | "=" | "like" 


 lazy val multivalueWithQuotes = valueWithQuotes ~ (rep((" " ~> ("or"|"and")) ~ (" " ~> valueWithQuotes) ^^ { case (fst:String) ~ (valu:String) => (" " + fst + " " + valu) } )) ^^ {
       case (first:String) ~ (second:List[String]) => (first::second).mkString("")
		}

 lazy val funcValue = "amount" ~ opt(space) ~ "(" ~ opt(space) ~ "'" ~> (amount <~ "'" ~ opt(space) ~ ")") ^^ { case (amnt:String) => "amount ("+amnt+")" }     

 lazy val critWithoutPar = (keyword <~ opt(space)) ~ operator ~ (opt(space) ~> (multivalueWithQuotes | funcValue | value)) ^^ {
                case (keyw:String) ~ (oper:String) ~ (vali:String) => {myA ! add(Criteria(keyw,oper,vali)) ; myA ! addMap(Criteria(keyw,oper,vali));  "yes"} }

 def initCriterias = myA ! InitCriterias
 def getCriterias = myA !? GetCriterias

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

 def showCriterias = myA ! ShowCriteria 

 def test(str:String) = { parseItem(str); showCriterias }

 
 def loadCriterias = {
   val srcxml = scala.io.Source.fromFile("C:/USERS/criterias_v7.xml").mkString
   val xmlData = XML.loadString(srcxml);
   println ((xmlData \\ "Criteria").map(i => i.text.length).mkString("\n")) 
   print ("nb criterias: " + (xmlData \\ "Criteria").length)
 }
 

 def checkCriterias : List[String] = { 
 var erreurs: List[String] = Nil
 val srcxml = scala.io.Source.fromFile("C:/USERS/criterias_v7.xml").mkString
   val xmlData = XML.loadString(srcxml);
  print ("nb criterias: " + (xmlData \\ "Criteria").length) 
  (xmlData \\ "Criteria").map(i => if (parseItem(i.text)==false) (erreurs = filterCriteria(i.text) :: erreurs))  
  erreurs 
 }


 


 val anyValue   = regex(new Regex(".+"))

 
 val lineCriteria  = "rule_criteria =" ~ (elem(' ')).*  ~> anyValue
 val lineEndCriteria = "rule_internal_criteria = " <~ anyValue 

 val lineRuleIntvText = "rule_rule_intv_text [0] = "  ~> anyValue
 val lineEndRuleIntvText = "rule_rule_intv_text [1] = "  ~> opt(anyValue)
 
 def parseNewIntv(line:String) = parse(lineRuleIntvText,line)
 def parseEndNewIntv(line:String) = parse(lineEndRuleIntvText,line)

 def parseCriteria(line: String) = parse(lineCriteria, line)
 def parseEndCriteria(line: String) = parse(lineEndCriteria, line) 
 

 def parseFile(i:Iterator[String]):List[Rule] = { 

 val rulesActor = new RulesActor()
 rulesActor.start

 val lineSchemaName = "scma_schema_name = " ~> letter ^^ (s => configActor ! SchemaName(s))
 val lineSchemaStatus = "scma_schema_status = SC_ACTIVE" ^^^ {configActor ! SchemaActive}
 
 val lineRpName = "rule_rp_name = " ~> anyValue ^^ (s => rulesActor ! RpName(s))
 val lineRtName = "rule_rt_name = " ~> anyValue ^^ (s => rulesActor ! RtName(s))
 val lineRuleSeqNbr = "rule_sequence_nbr = " ~> anyValue ^^ (s => rulesActor ! RuleSeqNbr(s))
 val lineSchemaMap = "rule_schema_map = " ~> opt(anyValue) ^^ (s => if (s.isDefined) rulesActor ! RuleSchemas(s.get))
 val lineRuleDescription = "rule_description = " ~> anyValue ^^ (s => rulesActor ! RuleDesc(s))
 val lineRuleMpfnResult = "rule_mpfn_result [" ~> (number <~ "] = R_") ~ anyValue ^^ { case n ~ s => if (s!="NONE") rulesActor ! RuleMpfnResult((n,s))}   

 val lineRuleSourceActionType = "rule_sensitive_action_type = " ~> anyValue ^^ ( s => rulesActor ! RuleSourceAction(s)) 
 val lineRuleTargetRpName = "rule_target_rp_name = " ~> opt(anyValue) ^^ ( s => if (s.isDefined) rulesActor ! RuleTargetRpName(s.get)) 

 val lineRuleInstType = "rule_inst_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewAction(s)) 
 val lineRuleInstRpName = "rule_inst_rp_name [0] = " ~> (rpname <~ opt(espace)) ^^ ( s => rulesActor ! RuleNewTargetRpName(s)) 
 val lineRuleInstActionType = "rule_inst_action_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewActionType(s)) 
 val lineRuleInstUnitName =  "rule_inst_unit_name [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewUnitName(s)) 

 val lineRuleToken = "rule_token = " ~> anyValue ^^ ( s => rulesActor ! AddRule ) 

 val lineDefRpName = "rp_name = " ~> anyValue ^^ (s => configActor ! UpdateRP(s))
 val lineDefRpAssignFunc = "rpoi_assigned_mpfn_name = " ~> anyValue ^^ (s => configActor ! UpdateAssignFunc(s))  

 val infosRule = ( lineRpName | lineRtName | lineRuleSeqNbr | lineSchemaMap | lineRuleDescription | lineDefRpName | lineDefRpAssignFunc | lineRuleMpfnResult | lineRuleSourceActionType | lineRuleTargetRpName | lineRuleInstType | lineRuleToken | lineRuleInstRpName |lineRuleInstActionType | lineRuleInstUnitName | lineSchemaStatus | lineSchemaName ) 
 
 def parseInfos(line : String) = parse (infosRule,line); 

 def itFile(a:(List[String],String),b:String):(List[String],String) = { 
 if (a._2.isEmpty) 
  {
   if (parseInfos(b).successful) (a._1,a._2)
   else if (parseCriteria(b).successful) (a._1,parseCriteria(b).get) 
        else if (parseNewIntv(b).successful) (a._1,parseNewIntv(b).get) 
             else(a._1,a._2)
  }
  else 
   {
    if (parseEndCriteria(b).successful) {initCriterias;parseItem(a._2);
                                         getCriterias match {
                                                             case l:List[Criteria] => rulesActor ! RuleCond((filterCriteria(a._2),l))
                                                            }
                                         ((a._2 :: a._1), "")} 
    else if (parseEndNewIntv(b).successful) {
                                             rulesActor ! RuleNewIntv(a._2)
					     (a._1, "")
                                            }
         else (a._1,a._2 + "\n" + b)
   }  

}
 
 var criterias:List[String] = Nil
 i.foldLeft(criterias,"")(itFile) 
 rulesActor !? GetRules match {
  case s: List[Rule] => s.reverse
  case _ => Nil
}


}


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

 def checkCriteriasV6 : List[String] = { 
   var erreurs: List[String] = Nil
   var criterias:List[String] = Nil
   val myIterator = scala.io.Source.fromFile("C:/USERS/configParis.out").getLines  
   val resu = myIterator.foldLeft(criterias,"")(parcours)
   print ("nb criterias: " +  resu._1.length) 

   resu._1.map(i => if (parseItem(i)==false) (erreurs = filterCriteria(i) :: erreurs))  
  erreurs 
 }
  
 def checkRulesV6(pathFile:String) : List[Rule] = {
  val rules = parseFile(scala.io.Source.fromFile(pathFile).getLines)  
  print ("nb rules" + rules.length)
  rules
}

def loadRules(configFile:String):List[Rule]  = {
  val rules = parseFile(scala.io.Source.fromFile(configFile).getLines)  
  print ("nb rules" + rules.length)
  rules
}

 
}

///////////////////////////////////////////////////////////////////////////////////////////////////

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

class Mesg(umid:String, creaDateTime:Date, receiver:String, sender:String, sens:Char, trn:String, mt:String) {
  def getUmid = umid
  def getCreaDateTime = creaDateTime
  def getReceiver = receiver
  def getSender = sender
  def getSens = sens
  def getTrn = trn
  def getMt = mt
}

class Inst(umid:String,num:Int,typ:String,relNum:Int,appeSeqNbr:Int,unit:String,creaRpName:String,creaDateTime:Date) extends Enr(umid) {
  def getNum = num 
  def getType= typ 
  def getRelNum = relNum
  def getAppeSeqNbr = appeSeqNbr
  def getUnit = unit
  def getCreaRpName = creaRpName
  def getCreaDateTime = creaDateTime
  override def toString = "umid:"+umid + " num:" + num + " appeSeqNbr:" + appeSeqNbr + " unit:" + unit + " creaRpName:" + creaRpName 
}

class Appe(umid:String,instNum:Int,seqNbr:Int,iappName:String,sessionHolder:String,creaRpName:String) extends Enr(umid) {
   def getInstNum = instNum
   def getSeqNbr = seqNbr
   def getIappName = iappName
   def getSessionHolder = sessionHolder
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

 val optConfig = Config.ConfigTool() 
 if (optConfig.isEmpty) System.exit(2)
 val config = optConfig.get

 lazy val rulesConfig = Criterias.checkRulesV6(config.getCfgSaaFile);

 val formatDatefromExtract = new SimpleDateFormat("dd/MM/yy HH:mm:ss");

 def convertToDate(s:String):Date = try { formatDatefromExtract.parse(s) } catch { case _ => print ("Erreur de conversion date["+ s + "]") ; new Date(0) }

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
 lazy val multiRpName = regex(new Regex("[a-zA-Z0-9_,]+"))
 lazy val spaceAndReturn = (elem(' ') | elem('\t') | elem('\r') | "\\r\\n").+
 lazy val rpName = regex(new Regex("[a-zA-Z0-9_]+")) <~ opt(spaceAndReturn)
 lazy val resultLabel = """([a-zA-Z0-9_ ]|-|/)+""".r
 lazy val funcName = regex(new Regex("[a-zA-Z0-9_]+"))
 lazy val unitName = regex(new Regex("[a-zA-Z0-9_]+")) <~ opt(spaceAndReturn) 

 val routedFromAction = ("Routed from rp [" ~>  rpName ) ~ ( "] to rp [" ~> (rpName <~ "]; " ))  ~ opt( (number ~ " instance(s) created at [") ~> ( multiRpName <~ "] respectively;")) ~ ( "On Processing by Function " ~> funcName ) ~ (" with result " ~> opt(resultLabel)) ~ (";(Rule:" ~> ("USER"|"SYSTEM")) ~ ("," ~> number) <~ ")"    

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

def parseIntv(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if (champs.length>17) if (champs(7) !="\"INTY_OTHER\"" && champs(7)!="\"INTY_DELIVERY_REPORT\"")  }
    yield new Intv(champs(1),champs(2).toInt,champs(4).toInt,champs(6),champs(17))
}


def checkIntv = {
 val intvs = parseIntv(scala.io.Source.fromFile(config.getIntvFile).getLines.drop(1))
  toMapByUmid(intvs)
}

def parseMesg(i:Iterator[String]) = {
 for {li <- i ; val champs =   li.split("\",").map(_.drop(1)) if champs.length>73 if champs(33)!="MX"}
               yield new Mesg(champs(1),convertToDate(champs(15)),champs(46),champs(65),champs(69).head,champs(72),champs(73))
}


def checkMesg = {
 val mesgs = parseMesg(scala.io.Source.fromFile(config.getMesgFile).getLines.drop(1))
 mesgs.map(me => (me.getUmid,me)).toMap 
}



lazy val instTypeParse = "INST_TYPE_" ~> "[A-Z]+".r 

def minimizeInstType(s:String) = parseAll(instTypeParse,s) match {
								case Success(res,_) => res
								case _ => s
							       }

def parseInst(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if champs.length>9 }
               yield new Inst(champs(1),champs(2).toInt,minimizeInstType(champs(3)),champs(6).toInt,champs(8).toInt,champs(9),champs(20),convertToDate(champs(21)))
}

def checkInst = {
 var instsMap = Map[String,List[Inst]]()
 val insts = parseInst(scala.io.Source.fromFile(config.getInstFile).getLines.drop(1))
 toMapByUmid(insts)
}

def toMapByUmid[A <: Enr](i:Iterator[A]) = {
  var instsMap = Map[String,List[A]]()
  val res = i.foldRight(("",List[A]()))((a,b) => if (a.getUmid!=b._1) {if (b._1!="") instsMap += (b._1 -> b._2) ; (a.getUmid,List[A](a)) } else (b._1,a :: b._2))
  if (res._1!="") instsMap + (res._1 -> res._2) else instsMap  
  
}

def parseAppe(i:Iterator[String]) = {
 for {li <- i ; val champs =  li.split("\",").map(_.drop(1)) if champs.length>14 }
               yield new Appe(champs(1),champs(2).toInt,champs(4).toInt,champs(5),champs(7),champs(13))
}

def checkAppe = {
 val appes = parseAppe(config.getAppeFile).getLines.drop(1)
 toMapByUmid(appes)
}

def getRules(rpNameSource:String,rpNameDest:String) = {
  rulesConfig.filter(r => r.rpName==rpNameSource && r.ruleNewTargetRpName==rpNameDest)
}


def intvWithRule = (routedFromAction | completedInAction)

lazy val AllIntvs = checkIntv 
lazy val AllInsts = checkInst
lazy val AllAppes = checkAppe

def getRulesFromIntvByMsgAndInst(umid:String,numInst:Int) = {
  val intvs = AllIntvs
  val intvsMsg = intvs.getOrElse(umid,Nil)
  val actMsg = new MsgActor
  val routes = intvsMsg.filter(intv => ( intv.getName=="Instance routed" || intv.getName=="Instance completed")   && intv.getInstNum==numInst )
 

 
  def parseRouteIntv(text:String) = {
                                     val myParse = parse(intvWithRule,text)
                                     myParse match {
					case Success(t,n) => {
                                     	        val resParse = myParse.get
				     		resParse match { 
                                     			case ((rpFrom:String) ~ (rpTo:String) ~ (instCreated:Option[String]) ~ (funcName:String) ~ (res:Option[String]) ~ (ruleType:String) ~ (ruleNumber:Int)) => Some(rpFrom,ruleNumber)
                                     			case ((rpName:String) ~ (funcName:String) ~ (res:Option[String]) ~ (ruleType:String) ~ (ruleNumber:Int)) => Some(rpName,ruleNumber)
				     			case _ => None
				    		}
					}
					case _ => None
				     }
}
 
 for { x <- routes ;val res = parseRouteIntv(x.getText) if res.isEmpty==false } yield res.get  
}



def getHiddenRules(umid:String,instNum:Int,rpNameSource:String,rpNameDest:String) = {
  val rules = getRules(rpNameSource,rpNameDest).map(x => (x.rpName,x.ruleSeqNbr.toInt))
  if (rules.length>1) { val rulesMsg = getRulesFromIntvByMsgAndInst(umid,0)
                        var rulesAfter = rules intersect rulesMsg; 
                        if (rulesAfter.length==0) rulesAfter = removeImpRules(rules,rulesMsg) 
			if (rulesAfter.length>1 && rpNameDest=="KR1pLOA") print ("[" + umid + "]\n") ; rulesAfter } else rules    
}

def removeImpRules(conf:List[(String,Int)],msgRules:List[(String,Int)]) = {
  conf.filter(_._2< msgRules.filter(_._1==conf(0)._1).map(_._2).max)
}

/////////// Path d'une instance de message

case class NewRpPath(rpFrom:String,rpTo:String,ruleNumber:Int)
case class CompletedInPath(rpCompleted:String,ruleNumber:Int)
case class CompletedAtPath(rpCompleted:String)
case class CreatedAtPath(rpCreated:String)
case object GetRpPath 






}




