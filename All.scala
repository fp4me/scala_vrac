import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor
import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import java.io.FileWriter

object Config {

class ConfigTool private (
        val cfgSaa:String,
	val mesgFile:String,
	val appeFile:String,
	val intvFile:String,
	val instFile:String,
	val textFile:String,
	val outFile:String) {

def getCfgSaaFile = cfgSaa
def getMesgFile = mesgFile
def getAppeFile = appeFile
def getTextFile = textFile
def getIntvFile = intvFile
def getInstFile = instFile
def getOutFile = outFile

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
			               toolProperties.getParameter("MESG_FILE"),
			               toolProperties.getParameter("APPE_FILE"),
				       toolProperties.getParameter("INTV_FILE"), 
				       toolProperties.getParameter("INST_FILE"),
				       toolProperties.getParameter("TEXT_FILE"),
				       toolProperties.getParameter("OUT_FILE"));
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
          case UpdateRP(s) => { if (s(0)=='H') println (s) ; currentRpName = s}
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
 var ruleTargetUnitName= "";
 var ruleNewIntv = "";
 var ruleTargetIntvText= "";
 var ruleInstNotificationType = "";
 var funcRes = Map.empty[Int,String]

 override def toString ="[" ++ rtName ++ "][" ++ rpName ++ ":" ++ ruleSeqNbr.toString ++ "] schemas[" ++ ruleSchemas ++ "]" ++ 
                        "\nDesc[" ++ ruleDesc ++ "]" ++
			printFuncRes ++ 
			"\nCondition:\n" ++ condition ++
			"\n" + printRouting 
 
  def printRule = rpName + "|" + 
                  ruleSeqNbr + "|" + 
                  ruleDesc +"|" + 
                  condition + "|" + 
                  ruleSourceAction + "|" + 
                  ruleTargetUnitName + "|" + 
                  ruleSourceTargetRpName + "|" + 
                  ruleTargetIntvText + "|" + 
                  ruleNewAction + "|" + 
                  ruleNewUnitName + "|" + 
                  (if (ruleNewAction=="INST_TYPE_NOTIFICATION") ruleInstNotificationType else "") + "|" + 
                  ruleNewTargetRpName + "|" + 
                  ruleNewIntv 

  

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
case class RuleTargetUnitName(s:String)
case class RuleNewUnitName(s:String)
case class RuleNewIntv(s:String)
case class RuleTargetIntvText(s:String)
case class RuleInstNotificationType(s:String)
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
    case RuleTargetUnitName(nut) => currentRule.ruleTargetUnitName = nut; 
    case RuleNewUnitName(un) => currentRule.ruleNewUnitName = un;
    case RuleNewIntv(intv) => currentRule.ruleNewIntv = intv;
    case RuleTargetIntvText(tit) => currentRule.ruleTargetIntvText = tit;
    case RuleInstNotificationType(it) => currentRule.ruleInstNotificationType = it;
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
                                //if (ConfigActor.getLabelFuncRes(currentRule.rpName,fr._2)==fr._2) println(ConfigActor.getAssignFunc(currentRule.rpName)+" "+fr._1+":"+ConfigActor.getLabelFuncRes(currentRule.rpName,fr._2))
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
 val lineRuleTargetUnitName = "rule_target_new_unit_name = " ~> opt(anyValue) ^^ ( s => if (s.isDefined) rulesActor ! RuleTargetUnitName(s.get)) 
 val lineRuleTargetIntvText = "rule_target_rule_intv_text = " ~> opt(anyValue) ^^ ( s => if (s.isDefined) rulesActor ! RuleTargetIntvText(s.get)) 

 val lineRuleInstType = "rule_inst_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewAction(s)) 
 val lineRuleInstNotificationType = "rule_inst_notification_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleInstNotificationType(s)) 
 val lineRuleInstRpName = "rule_inst_rp_name [0] = " ~> (rpname <~ opt(espace)) ^^ ( s => rulesActor ! RuleNewTargetRpName(s)) 
 val lineRuleInstActionType = "rule_inst_action_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewActionType(s)) 
 val lineRuleInstUnitName =  "rule_inst_unit_name [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewUnitName(s)) 

 val lineRuleToken = "rule_token = " ~> anyValue ^^ ( s => rulesActor ! AddRule ) 
 val lineRecordVersion = "rule_recordversion = " ~> anyValue ^^ ( s => rulesActor ! AddRule ) 

 val lineDefRpName = "rp_name = " ~> anyValue ^^ (s => configActor ! UpdateRP(s))
 val lineDefRpAssignFunc = "rpoi_assigned_mpfn_name = " ~> anyValue ^^ (s => configActor ! UpdateAssignFunc(s))  

 val infosRule = ( lineRpName | lineRtName | lineRuleSeqNbr | lineSchemaMap | lineRuleDescription | lineDefRpName | lineDefRpAssignFunc | lineRuleMpfnResult | lineRuleSourceActionType | lineRuleInstNotificationType | lineRuleTargetRpName | lineRuleInstType | lineRuleToken | lineRecordVersion | lineRuleInstRpName |lineRuleInstActionType | lineRuleTargetIntvText | lineRuleInstUnitName | lineRuleTargetUnitName | lineSchemaStatus | lineSchemaName ) 
 
 def parseInfos(line : String) =  parse (infosRule,line.filter((c:Char) => { if (c=='\n' || c==10) print (line + "\n") ; (c!='\n' && c!=10)})) 

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

 val myIterator = scala.io.Source.fromFile(new File("C:/USERS/configParis.out"),"ISO-8859-1").getLines   
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
 val rules = parseFile(scala.io.Source.fromFile(new File(pathFile),"ISO-8859-1").getLines)   
  rules
}

def loadRules(configFile:String):List[Rule]  = {
  val rules = parseFile(scala.io.Source.fromFile(configFile).getLines)  
  rules
}

 
}

///////////////////////////////////////////////////////////////////////////////////////////////////

case class Instance(typ:String,num:Int) 


class Enr(umid:String) {
 def getUmid = umid
}



class Intv( umid:String,instNum:Int,seqNbr:Int,name:String,text:String) extends Enr(umid) {
  def getInstNum = instNum 
  def getSeqNbr = seqNbr
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

class Text(umid:String,bloc4:String) extends Enr(umid) {
  def getBloc4=bloc4;
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

def checkInstances = {
  val insts = parseFile(scala.io.Source.fromFile("C:/USERS/mt543.hist").getLines)  
  insts
}


def splitLine(s:String) = s.split("\",").map(_.drop(1))

def parseIntv(i:Iterator[String]) = {
 for {li <- i ; val champs = splitLine(li) if (champs.length>17) if (champs(7)!="INTY_MESG_MODIFIED" && champs(7)!="INTY_OTHER" && champs(7)!="INTY_DELIVERY_REPORT")  }
    yield new Intv(new String(champs(1)),champs(2).toInt,champs(4).toInt,new String(champs(6)),new String(champs(17)))
}

def checkIntv = {
 val intvs = parseIntv(scala.io.Source.fromFile(config.getIntvFile).getLines.drop(1))
 toMapByUmid(intvs)
}

def parseMesg(i:Iterator[String]) = {
 for {li <- i ; val champs = splitLine(li) if champs.length>73 if champs(33)!="MX"}
               yield new Mesg(new String(champs(1)),convertToDate(champs(15)),new String(champs(46)),new String(champs(65)),champs(69).head,new String(champs(72)),new String(champs(73)))
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
               yield new Inst(new String(champs(1)),champs(2).toInt,minimizeInstType(champs(3)),champs(6).toInt,champs(8).toInt,new String(champs(9)),new String(champs(20)),convertToDate(champs(21)))
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
               yield new Appe(new String(champs(1)),champs(2).toInt,champs(4).toInt,new String(champs(5)),new String(champs(7)),new String(champs(13)))
}

def checkAppe = {
 val appes = parseAppe(scala.io.Source.fromFile(config.getAppeFile).getLines.drop(1))
 toMapByUmid(appes)
}

def getRules(rpNameSource:String,rpNameDest:String) = {
  rulesConfig.filter(r => r.rpName==rpNameSource && r.ruleNewTargetRpName==rpNameDest)
}


def intvWithRule = (routedFromAction | completedInAction)

lazy val AllIntvs = { print ("\nIntv File Loading...") ; val res = checkIntv ; print(" Done") ; res} 
lazy val AllInsts = { print ("\nInst File Loading...") ; val res = checkInst ; print(" Done") ; res} 
lazy val AllAppes = { print ("\nAppe File Loading...") ; val res = checkAppe ; print(" Done") ; res}



/////////// Path d'une instance de message

case class NewRpPath(rpFrom:String,rpTo:String,ruleNumber:Int)
case class CompletedInPath(rpCompleted:String,ruleNumber:Int)
case class CompletedAtPath(rpCompleted:String)
case class DisposedFrom(rpDisposed:String)
case class CreatedAtPath(rpCreated:String)
case object GetRpPath 

class PathInst (pUmid:String,pNumInst:Int) extends Actor
{
 val umid = pUmid
 val numInst = pNumInst
 var rpCreated = "";
 var rpPath = List[(String,Option[(String,Int)])]()
 var rpCompleted = "";
 
 def act = loop {
   react {
     case NewRpPath(rpFrom,rpTo,ruleNumber) => rpPath = (rpTo,Some(rpFrom,ruleNumber)) :: rpPath 
     case CompletedInPath(rp,ruleNumber) => { rpCompleted =  rp ; rpPath = (rp,Some(rp,ruleNumber)) :: rpPath }
     case DisposedFrom(rp) => { rpPath = (rp,None) :: rpPath }
     case CompletedAtPath(rp) => { rpCompleted =  rp ; rpPath = (rp,None) :: rpPath }
     case CreatedAtPath(rp) => { rpCreated = rp ; rpPath = ((rp,None) :: rpPath)}
     case GetRpPath => reply (rpPath.reverse) 
}

}
}

def getPathInst(umid:String,numInst:Int) = {

val pathInst = new PathInst(umid,numInst) 
pathInst.start

 val routedFrom = ( "Routed from rp [" ~>  rpName ) ~  
                  ( "] to rp [" ~> (rpName <~ "]; " ))  ~   
                  (
                   (
                    opt( (number ~ " instance(s) created at [") ~> ( multiRpName <~ "] respectively;")) ~
                    ( "On Processing by Function " ~> funcName ) ~
                    ( " with result " ~> opt(resultLabel)) ~
                    ( ";(Rule:" ~> ("USER"|"SYSTEM"|"DEFAULT_RULE") )
		   ) ~> 
                   (("," ~> number) <~ ")") 
                  ) ^^ {
	                case (rpFrom:String) ~ (rpTo:String) ~ (ruleNumber:Int) => pathInst ! NewRpPath(rpFrom,rpTo,ruleNumber)
		       }
 
 val disposedFrom =  "Disposed from rp [" ~  rpName  ~ "] to rp [" ~> rpName <~ "]; "  ~ "On processing by Function " ~ 
                      funcName ~ " with result " ~ opt(resultLabel) ~ ";" ^^ (rp => pathInst ! DisposedFrom(rp))

 val completedIn = ("Completed in rp [" ~>  rpName ) ~ 
                   (
                    (
                     ( "]; On Processing by Function " ~> funcName ) ~
                     (" with result " ~> opt(resultLabel)) ~ 
                     (";(Rule:" ~> ("USER"|"SYSTEM"|"DEFAULT_RULE") ) 
                    ) ~>
                    (("," ~> number) <~ ")")
                  ) ^^ {
                        case (rp:String) ~  (ruleNumber:Int) =>  pathInst ! CompletedInPath(rp,ruleNumber)  
                       }


 val manuallyCompleted = ("Manually completed from rp [" ~> (rpName <~ "]" )) <~  opt( " and assigned to unit [" ~ unitName ~ "]" ) ^^ ( rp => pathInst ! CompletedAtPath(rp))
 
 val manuallyDisposed = "Manually disposed from rp [" ~ rpName ~ "]" ~ " to rp [" ~> rpName <~ "]" ^^ (rp => pathInst ! DisposedFrom(rp))

 val completedAt =("Completed at rp [" ~> rpName ) <~ "]" ^^ ( rp => pathInst ! CompletedAtPath(rp))

 val createdAt = ( "Created at rp [" ~>  rpName) <~  ( "] and assigned to unit [" ~ unitName ~ "]" ) ^^ ( rp => pathInst ! CreatedAtPath(rp))

 def pathRule = (createdAt | routedFrom | completedIn | completedAt |  manuallyCompleted | disposedFrom |manuallyDisposed)

 
  val intvsMsg = AllIntvs.getOrElse(umid,Nil)
  val intvsPath = intvsMsg.filter(intv => ( intv.getName=="Instance created" || intv.getName=="Instance routed" || intv.getName=="Instance completed") && intv.getInstNum==numInst )
 

  def parseIntvPath(text:String) = {
                                     val myParse = parse(pathRule,text)
                                     myParse match {
					case Success(t,n) => {}
					case _ => print ("Error on parse:[" + text + "]") 
				     }
}
  
 intvsPath foreach ( x => parseIntvPath(x.getText) )
 val resu = pathInst !? GetRpPath
 resu match {
      case l:List[(String,Option[(String,Int)])] => if (l.length==0) print ("inst["+umid+"]["+numInst+"]\n");l
      case _ => List[(String,Option[(String,Int)])]()
  }


}

def allPathInst = { 
  val insts = AllInsts.toIterator
  for { instsbyUmid <- AllInsts ; inst <- instsbyUmid._2.toIterator; if (inst.getNum!=0) } yield getPathInst(inst.getUmid,inst.getNum) 
  //for { instsbyUmid <- AllInsts ; inst <- instsbyUmid._2.toIterator; if (inst.getNum!=0) } getPathInst(inst.getUmid,inst.getNum)
}

type IntString = (Int,String)

class MsgInAndOuts private (
   val rpIn:String,
   val mqIn:Option[String],
   val rpOut:List[IntString],
   val apOut:List[IntString],
   val mqOut:List[IntString],
   val appCode:List[IntString],
   val lastRule:List[IntString],
   val completedRule:List[IntString]
) {

override def toString = {
  
  (if (mqIn.isDefined)  "Mesg Received from MQ["+ mqIn.get + "]\n" else "") +  
  "Mesg Created in:" +rpIn + "\n" +
  rpOut.foldRight("")((rp,out) => "Mesg Inst[" + rp._1 + "] completed in:" + rp._2 + "\n" + out) + 
  apOut.foldRight("")((ap,out) => "Mesg Inst[" + ap._1 + "] send to APPLI[" + ap._2 + "]\n" + out) + 
  mqOut.foldRight("")((mq,out) => "Mesg Inst[" + mq._1 + "] send to MQ[" + mq._2 + "]\n" + out)
}

}




object MsgInAndOuts {

def apply(umid:String) = {
  val insts = AllInsts
  val appes = AllAppes
  val instsMsg = insts.getOrElse(umid,Nil)
  val appesMsg = appes.getOrElse(umid,Nil)

  var rpIn="" 
  var mqIn:Option[String]=None 
  var rpOut=List[IntString]()
  var appCode=List[IntString]()
  var apOut=List[IntString]() 
  var mqOut=List[IntString]() 
  var lastRule=List[IntString]() 
  var completedRule=List[IntString]() 

 
  
  instsMsg foreach ( x => { 

                            val intvsMsgForMQSA = AllIntvs.getOrElse(umid,Nil).filter(_.getInstNum==x.getNum).filter(intv => ( intv.getName=="Rule rule_text" && intv.getText.contains("MQSA-ROUTINGCODE")))
                           if (intvsMsgForMQSA.length>0)
                             {
                              val intv = intvsMsgForMQSA.filter(_.getInstNum==x.getNum).reduce((a,b) => if (a.getSeqNbr>b.getSeqNbr) a else b)
                              appCode ::= (x.getNum,getApplRoutingCode(intv.getText)) 
                             }

                           val rpPath = getPathInst(umid,x.getNum) 

                           rpPath.length match {
				case 0 => {}
			        case (nb:Int) => {
                                                if (x.getNum==0) rpIn = rpPath(0)._1             
                                                rpOut ::= (x.getNum,rpPath.last._1)
                                                rpPath.last._2 match 
                                                      {
                                                       case None => {}
					               case Some(r) =>  completedRule ::= (x.getNum,"USER"+r)
						      }
				                if (nb>1) rpPath.init.foldLeft(None:Option[(String,Int)])((lastOne,routing) => if (!routing._2.isEmpty) routing._2 else lastOne) match
							{ case Some(r) => lastRule ::= (x.getNum,"USER"+r)
							  case None => {}
							}
						}
                          }} )

  appesMsg foreach ( x => { if ((x.getIappName=="SMQS") && (x.getCreaRpName=="SMQS_To_MQSeries"))  mqOut ::=  (x.getInstNum,x.getSessionHolder) 
                           if ((x.getIappName=="SMQS") && (x.getCreaRpName=="SMQS_From_MQSeries")) mqIn = Some(x.getSessionHolder) 
			    if ((x.getIappName=="APPLI")) apOut ::= (x.getInstNum,x.getSessionHolder) 
                          })



 new MsgInAndOuts(rpIn,mqIn,rpOut.sortWith(_._1<_._1),apOut.sortWith(_._1<_._1),mqOut.sortWith(_._1<_._1),appCode.sortWith(_._1<_._1),lastRule.sortWith(_._1<_._1),completedRule.sortWith(_._1<_._1))

}

}


lazy val AllRules = { print ("\nConfig File Loading...") ; val res = rulesConfig ; print(" Done") ; res  } 
lazy val AllMesgs = { print ("\nMesg File Loading...") ; val res = History.checkMesg ; print(" Done") ; res }
lazy val AllMesgsInList = AllMesgs.values.toList

lazy val currentSchema = (ConfigActor !? GetSchemaActive) match {
                                case (s:String) => s
				case _ => {print ("Pb no current schema") ; "?"}
			}

lazy val AllRulesWithNewTargetRp = AllRules.filter(_.ruleNewTargetRpName.length>0).filter(r => (r.rtName!="USER") || (r.ruleSchemas contains currentSchema))  
lazy val AllRulesToSMQS = AllRulesWithNewTargetRp.filter(_.ruleNewTargetRpName=="SMQS_To_MQSeries") 
lazy val AllRulesToAppli = AllRulesWithNewTargetRp.map(x => (x.rpName,x.ruleSeqNbr,x.ruleNewTargetRpName))


abstract class Routing(rpFrom:String,rpTo:String,idRule:Int) {
	def getRpTo = rpTo
	val getRule:String
}

class PossibleRouting(rules:List[Routing]) {
 def getRules= rules 
}


case class RoutingByUserRule(val rpFrom:String,val rpTo:String,val idRule:Int) extends Routing(rpFrom,rpTo,idRule)
 { val getRule = "USER("+rpFrom+","+idRule+")" } 
case class RoutingBySystemRule(val rpFrom:String,val rpTo:String,val idRule:Int) extends Routing(rpFrom,rpTo,idRule)
 { val getRule = "SYSTEM("+rpFrom+","+idRule+")" } 

lazy val AllRoutingObj = AllRulesWithNewTargetRp.map(x =>  x.rtName match {
                                                           case "USER" => new RoutingByUserRule(x.rpName,x.ruleNewTargetRpName,x.ruleSeqNbr)
							   case _ => new RoutingBySystemRule(x.rpName,x.ruleNewTargetRpName,x.ruleSeqNbr)
				                          })


val anyValueOrNothing   = regex(new Regex(".*")) 
lazy val rCode = regex("<".r) ~> (regex("[^>]+".r) <~ ( regex(">".r) ~ anyValueOrNothing)) 

def getApplRoutingCode(s:String) = {

val appls=(for { i <- s.split("APPL-ROUTINGCODE=") ; res = parse(rCode,i); if res.successful } yield res.get);

appls.length match {
	case 0 => "";
	case n => appls(0); 
}

}

def getRuleFromNewInst(umid:String,idInst:Int) = {

val insts = AllInsts
val appes = AllAppes
lazy val intvsMsgForMQSA = AllIntvs.getOrElse(umid,Nil).filter(intv => ( intv.getName=="Rule rule_text" && intv.getText.contains("MQSA-ROUTINGCODE")))
 
val instsMsg = insts.getOrElse(umid,Nil)
val instsTmp = instsMsg.filter(_.getNum==idInst)


if (instsTmp.length!=1 && idInst!=0) None
else {
      val inst = instsTmp(0)     
      val instOrig = inst.getRelNum
      inst.getCreaRpName match {
         case "SMQS_To_MQSeries" => {
                     
                     if (intvsMsgForMQSA.length==0) None
                     else {
                           val intv = intvsMsgForMQSA.filter(_.getInstNum==idInst).reduce((a,b) => if (a.getSeqNbr>b.getSeqNbr) a else b)
                             
                           val possiblesRules = AllRulesToSMQS.filter(_.ruleNewIntv==intv.getText.replace("\\n","\n")).map(x => (x.rpName,x.ruleSeqNbr))                         
                           val pathInstOrig = getPathInst(umid,instOrig).map(_._2)
                           val res = (possiblesRules.foldLeft(List[Routing]())((l,r) => 
                                       if (pathInstOrig.filter(_!=None).map(_.get).filter(_._1==r._1).exists(_._2>=r._2)) new RoutingByUserRule(r._1,"SMQS_To_MQSeries",r._2) :: l else l)
                                     ).distinct
                           if (res.length>0) Some(new PossibleRouting(res)) else None   
                          }
                   }
         case "" => None
         case (rp:String) => {

                    val possiblesRules = AllRoutingObj.filter(_.getRpTo==rp) 
		    val pathInstOrig = getPathInst(umid,instOrig).map(_._2)

		    val res = (possiblesRules.foldLeft(List[Routing]())((l,r) => { 
                                          val routingPathInst = pathInstOrig.filter(_!=None).map(_.get)
                                          r match {
                                                   case RoutingBySystemRule(rpFrom,_,_) => if (routingPathInst.filter(_._1==rpFrom).length>0)  r :: l else l  
                                                   case RoutingByUserRule(rpFrom,_,idRule) => if (routingPathInst.filter(_._1==rpFrom).exists(_._2 >= idRule))  r :: l else l
                                                  } 
                                                                                  } )
                                     ).distinct
                    if (res.length>0) Some( new PossibleRouting(res)) else None   
                   }
         
        }
            

     }

}

def readRequest():String = {

val c = Console.in.read.toChar
Console.print (c)
if (c.toInt==8)  {Console.print (' ') ; Console.print (8.toChar) }
if (c.toInt!=13&&c.toInt!=10) c + readRequest
else ""
}


def readFromConsole():String = {
(readRequest.foldRight(("",0))((c,res) => if (c.toInt==8) (res._1,res._2+1) else
                                              if (res._2>0) (res._1,res._2 -1) else
                                                  (c+res._1,res._2)))._1

}

def filterOnMt(criteria:String):(Mesg => Boolean) = m => m.getMt matches criteria 
def filterOnTrn(criteria:String):(Mesg => Boolean) = m => m.getTrn matches criteria 
def filterOnReceiver(criteria:String):(Mesg => Boolean) = m => m.getReceiver matches criteria 
def filterOnSender(criteria:String):(Mesg => Boolean) = m => m.getSender matches criteria 
def filterOnSens(criteria:Char):(Mesg => Boolean) = m => m.getSens==criteria 
def filterOnDate(operand:String,date:Date):(Mesg => Boolean) = m => { val res = m.getCreaDateTime.compareTo(date); operand match {
										case ">" => res > 0    
										case ">=" => res >= 0    
										case "<" => res < 0    
										case "<=" => res <= 0    
										case "=" => res == 0   
                                                                               }}
								       
implicit def listToListWithfilter(input: List[Mesg]) = new ListWithFilter(input)

class ListWithFilter(val source: List[Mesg]) {
     def applyCriteria(f:(Mesg) => Boolean):List[Mesg] = source.filter(f)
    }

def TestList : List[Mesg] =  History.checkMesg.values.toList applyCriteria filterOnMt("300") applyCriteria filterOnSender("SOGEKR(.*)") applyCriteria filterOnDate(">",formatDatefromExtract.parse("22/12/11 07:00:00"))


def getRequest() = {

lazy val trnValue   = regex(new Regex("[a-zA-Z0-9-_%/.!: ]+"))
lazy val searchValue   = "[0-9a-zA-Z%_]+".r
lazy val senderValue   = searchValue 
lazy val receiverValue   = searchValue
lazy val sensValue   = ("I"|"O")
lazy val mtValue   = searchValue
lazy val espace = (elem(' ')).+

def returnParse[T](value:T):Parser[T] = new Parser[T] {def apply(in:Input) = Success(value,in)}

val formatDatefromExtract = new SimpleDateFormat("dd/MM/yy HH:mm:ss")

val errorOnDay = failure("day must be in range[01-31]")
val formatDay : Parser[String]  = ("[0-3][0-9]".r|errorOnDay) into  (s => if (s.toInt>31) errorOnDay else returnParse(s) )

val errorOnMonth = failure("month must be in range[01-12]")
val formatMonth : Parser[String]  = ("[0-1][0-9]".r| errorOnMonth) into (s => if (s.toInt>12) errorOnMonth else returnParse(s) )

val errorOnYear = failure("day must be in range[00-99]")
val formatYear : Parser[String]  = ("[0-9][0-9]".r|errorOnYear) 

val errorOnHH = failure("HH must be in range[00-23]")
val formatHH : Parser[String]  = ("[0-2][0-9]".r|errorOnHH) into (s => if (s.toInt>23) errorOnHH else returnParse(s) )

val errorOnmm = failure("mm must be in range[00-59]")
val formatmm : Parser[String]  = ("[0-5][0-9]".r|errorOnmm) into (s => if (s.toInt>59) errorOnmm else returnParse(s) )

val errorOnss = failure("ss must be in range[00-59]")
val formatss : Parser[String]  = ("[0-5][0-9]".r|errorOnss) into (s => if (s.toInt>59) errorOnss else returnParse(s) )

val errorOnDate = failure("invalid date check format[dd/MM/yy HH:mm:ss]")

val formatDateAndTime = formatDay ~ ("/" ~> formatMonth) ~ ("/" ~> formatYear)  ~ (" " ~> formatHH)  ~ (":" ~> formatmm)  ~ (":" ~> formatss) into { case (d:String) ~ (mo:String) ~ (y:String) ~ (h:String) ~ (m:String) ~ (s:String) => try { returnParse(formatDatefromExtract.parse(d+"/"+mo+"/"+y+" "+h+":"+m+":"+s)) } catch { case _ => errorOnDate } } 

val errorOnFilter = failure("Unknown filter Criteria\n Possibles criterias:\n DATE TRN SENDER RECEIVER SENS MT")
val errorOnSelect = failure("SELECT syntax : SELECT CRITERIA=VALUE ... \n Possibles criterias:\n DATE TRN SENDER RECEIVER SENS MT")

def convertToFilterSaa(s:String) = s.replaceAll("%",".*").replaceAll("_",".") 

val operatorCompare = (">="|">"|"<="|"<"|"=")   
val criteriaOnDate = ("DATE" ~> operatorCompare) ~ ("\"" ~> (formatDateAndTime|errorOnDate)  <~ "\"") ^^ { case (op:String) ~ (d:Date) => filterOnDate(op,d) }   
val criteriaOnTrn = "TRN=\"" ~> (trnValue <~ "\"") ^^ (s => filterOnTrn(convertToFilterSaa(s))) 
val criteriaOnSender = "SENDER=\"" ~> (senderValue <~ "\"") ^^ (s => filterOnSender(convertToFilterSaa(s)))   
val criteriaOnReceiver = "RECEIVER=\"" ~> (receiverValue <~ "\"") ^^ (s => filterOnReceiver(convertToFilterSaa(s)))   
val criteriaOnSens = "SENS=\"" ~> (sensValue <~ "\"")  ^^ (s => filterOnSens(s.head)) 
val criteriaOnMt = "MT=\"" ~> (mtValue <~ "\"")  ^^ (s => filterOnMt(convertToFilterSaa(s)))  

val filterMsg = (criteriaOnTrn|criteriaOnDate|criteriaOnSender|criteriaOnReceiver|criteriaOnSens|criteriaOnMt|errorOnFilter)
val selectCommand = (opt(espace)  ~ ("SELECT"|"select"))  ~> (rep1 (espace ~> filterMsg)|errorOnSelect)
val input = readFromConsole
print (input+"\n:")

val filters = parseAll(selectCommand,input).getOrElse(List[Mesg => Boolean]())
parseAll(selectCommand,input) match {
 case Success(res,_) => {
                          val mesgs = AllMesgsInList
			  res.foldLeft(mesgs)((l,f) => l applyCriteria f) 
			 }
 case NoSuccess(msg,_) => { println (msg) ; List[Mesg]() }
}

}

def printAllMsgInCsv() = {
val labels =  List("umid","inst","datetime","type","mt","trn","io","sender","receiver","unit","ruleFrom","rpin","mqin","out","lastRule","completedRule")
var resu = List[List[String]]()
List(labels) :::  AllMesgsInList.foldLeft(resu)( (last,msg) => printMsgInCsv(msg.getUmid).get ::: last) 

}


def removeLTFromBic12(s:bic) = (s.length() >= 12) match 
                               {
                                true => s.take(8) + s.substring(9)                
                                false => s
                               } 

def printMsgInCsv(umid:String) =  {
AllMesgs.get(umid) match {
 case None => None
 case Some(mesg) => 	{
             val inAndOuts = MsgInAndOuts(umid)   
             val insts = AllInsts.getOrElse(umid,Nil)
             
             var lines:List[List[String]] = List()
             
             insts.foreach { inst => {
             	val possibleRoutingFrom= getRuleFromNewInst(umid,inst.getNum)

                val ruleFroms = possibleRoutingFrom match {
                                  case Some(p:PossibleRouting) =>  p.getRules 
                                  case _ => Nil
                                  }

                val ruleFrom = ruleFroms.length match {
						    case 0 => ""
						    case _ => ruleFroms.map(_.getRule).mkString("|")
                                	                   }
              
             	val mqIn = inAndOuts.mqIn.getOrElse("")
                
                val appCode = inAndOuts.appCode.filter(_._1==inst.getNum).headOption match {
					   case Some(a:IntString) if a._2.length>0 => "<APP="+a._2+">"  
                                           case _ => ""
					}

             	val mqOut = inAndOuts.mqOut.filter(_._1==inst.getNum).map("MQ:"+_._2+appCode)
	     	val apOut = inAndOuts.apOut.filter(_._1==inst.getNum).map("AP:"+_._2)
	     	val lastRule = inAndOuts.lastRule.filter(_._1==inst.getNum).map(_._2) match {
                             case Nil => ""
			     case (x :: xs) => x
				}
		val completedRule = inAndOuts.completedRule.filter(_._1==inst.getNum).map(_._2) match {
                             case Nil => ""
			     case (x :: xs) => x
				}

	     	val outs = (mqOut ::: apOut).mkString("&")

             	val line:List[String] =  List(umid,inst.getNum.toString, formatDatefromExtract.format(inst.getCreaDateTime), inst.getType, mesg.getMt,mesg.getTrn, mesg.getSens.toString,removeLTFromBic12 mesg.getSender,  removeLTFromBic12 mesg.getReceiver, inst.getUnit,ruleFrom,inst.getCreaRpName,mqIn,outs,lastRule,completedRule)           
                lines = line :: lines                         
               }}
 Some(lines)
}

}}



def printMsgInXml(umid:String) = { 
             
      
AllMesgs.get(umid) match {
 case None => None
 case Some(mesg) => 	{
             val inAndOuts = MsgInAndOuts(umid)      
             Some(
                  <mesg id={umid.toString}>
                  <mt>{mesg.getMt}</mt>
		  <trn>{mesg.getTrn}</trn>
		  <sens>{mesg.getSens}</sens>
		  <sender>{mesg.getSender}</sender>
		  <receiver>{mesg.getReceiver}</receiver>
		  <creationDate>{formatDatefromExtract.format(mesg.getCreaDateTime)}</creationDate>
                  <creationRp>{inAndOuts.rpIn}</creationRp>
                  {if (inAndOuts.mqIn.isDefined) <fromMQ>{inAndOuts.mqIn.get}</fromMQ>}
                  {printInstInXml(umid,0)}
                  </mesg>)
                  }
		
}
}


def addChild(n:Node,c:Node):Node = n match { case e:scala.xml.Elem => e.copy(child=e.child++c) }


def printInstInXml(umid:String,inst:Int) =  {
  val path = getPathInst(umid,inst)
  var nodePath:Node = <path></path>
  for {routing <- path.tail ; val rpTo = routing._1 ; val rule = routing._2.getOrElse(("",0)) } {  
                              val ruleNode =  <routing to={rpTo}>
                                              <rule rp={rule._1} id={rule._2.toString}></rule>
			                      </routing>
                              nodePath = addChild(nodePath,ruleNode)
                             }                        

  <inst id={inst.toString}>
  <creationRp>{path.head._1}</creationRp>
  {nodePath}                  
  </inst>
 }



def getUnsupportedFunction() = {

val res = List("MAC No Keys","Mac Authentication Error","PAC No Keys","PAC Authentication Error","Authorisation/Bilateral key not present", "MAC No Keys" , "MAC Future Key" , "MAC Previous Key" , "PAC No Keys" , "PAC Future Key" , "PAC Previous Key" , "MAC Authentication Error" , "PAC Authentication Error" , "097 PAC Authentication Error" , "097 PAC No Keys" , "Authorisation/Bilatery key not present" ) 

res
}
}




object MainProg {

def main(args:Array[String]) = {
val optionConfig = Config.ConfigTool() 
if (optionConfig == None) 
 {
  print ("Error on loading config [tool.properties]");
  System.exit(1)
 }  
val config = optionConfig.get
val file = new FileWriter(config.getOutFile)

val listNeededFile = List(config.getInstFile,
                          config.getIntvFile,
                          config.getMesgFile,
                          config.getAppeFile,
                          config.getCfgSaaFile)

if (listNeededFile.exists(file => if (new File(file).exists) false else {print ("Invalid config, file["+file+"] not found"); true})) System.exit(1)

val all = History.printAllMsgInCsv()
all.map(l => file.write(l.mkString(";")+"\n"))
file.close
print("\nexport to file [" + config.getOutFile +"] Done")
System.exit(0)
}

def toCsv(fileOut:String) = {
val optionConfig = Config.ConfigTool() 
if (optionConfig == None) 
 {
  print ("Error on loading config [tool.properties]");
  System.exit(1)
 }  

val file = new FileWriter(fileOut)
val all = History.printAllMsgInCsv()
all.map(l => file.write(l.mkString(";")+"\n"))
file.close
}

}
