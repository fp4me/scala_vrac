import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor


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

def act = loop {
   react {
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
 var ruleSeqNbr:String="";
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

 override def toString ="[" ++ rtName ++ "][" ++ rpName ++ ":" ++ ruleSeqNbr ++ "] schemas[" ++ ruleSchemas ++ "]" ++ 
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
var rules:List[Rule] = Nil;
var currentRule = new Rule

def act = loop {
   react {
    case RpName(rpn) => currentRule.rpName = rpn;
    case RtName(rtn) => currentRule.rtName = rtn;
    case RuleSeqNbr(rsn) => currentRule.ruleSeqNbr  = rsn;
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
 
 val lineRpName = "rule_rp_name = " ~> anyValue ^^ (s => rulesActor ! RpName(s))
 val lineRtName = "rule_rt_name = " ~> anyValue ^^ (s => rulesActor ! RtName(s))
 val lineRuleSeqNbr = "rule_sequence_nbr = " ~> anyValue ^^ (s => rulesActor ! RuleSeqNbr(s))
 val lineSchemaMap = "rule_schema_map = " ~> opt(anyValue) ^^ (s => if (s.isDefined) rulesActor ! RuleSchemas(s.get))
 val lineRuleDescription = "rule_description = " ~> anyValue ^^ (s => rulesActor ! RuleDesc(s))
 val lineRuleMpfnResult = "rule_mpfn_result [" ~> (number <~ "] = R_") ~ anyValue ^^ { case n ~ s => if (s!="NONE") rulesActor ! RuleMpfnResult((n,s))}   

 val lineRuleSourceActionType = "rule_sensitive_action_type = " ~> anyValue ^^ ( s => rulesActor ! RuleSourceAction(s)) 
 val lineRuleTargetRpName = "rule_target_rp_name = " ~> opt(anyValue) ^^ ( s => if (s.isDefined) rulesActor ! RuleTargetRpName(s.get)) 

 val lineRuleInstType = "rule_inst_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewAction(s)) 
 val lineRuleInstRpName = "rule_inst_rp_name [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewTargetRpName(s)) 
 val lineRuleInstActionType = "rule_inst_action_type [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewActionType(s)) 
 val lineRuleInstUnitName =  "rule_inst_unit_name [0] = " ~> anyValue ^^ ( s => rulesActor ! RuleNewUnitName(s)) 

 val lineRuleToken = "rule_token = " ~> anyValue ^^ ( s => rulesActor ! AddRule ) 

 val lineDefRpName = "rp_name = " ~> anyValue ^^ (s => configActor ! UpdateRP(s))
 val lineDefRpAssignFunc = "rpoi_assigned_mpfn_name = " ~> anyValue ^^ (s => configActor ! UpdateAssignFunc(s))  

 val infosRule = ( lineRpName | lineRtName | lineRuleSeqNbr | lineSchemaMap | lineRuleDescription | lineDefRpName | lineDefRpAssignFunc | lineRuleMpfnResult | lineRuleSourceActionType | lineRuleTargetRpName | lineRuleInstType | lineRuleToken | lineRuleInstRpName |lineRuleInstActionType | lineRuleInstUnitName ) 
 
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
  
 def checkRulesV6 : List[Rule] = {
  val rules = parseFile(scala.io.Source.fromFile("C:/USERS/configParis.out").getLines)  
  print ("nb rules" + rules.length)
  rules
}

def checkRulesV6Eurasie : List[Rule] = {
  val rules = parseFile(scala.io.Source.fromFile("C:/USERS/configEurasie.out").getLines)  
  print ("nb rules" + rules.length)
  rules
}
 
}


