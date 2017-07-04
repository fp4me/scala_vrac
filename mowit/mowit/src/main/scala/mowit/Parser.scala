package mowit

import scala.util.matching.Regex
import scala.util.parsing.combinator._


class Parser extends RegexParsers {  
  
private val eoi = """\z""".r  // end of input
private val eol  = "\r\n" | "\n"  // end of line
private val separator = eoi | eol 

override val skipWhitespace = false
lazy val orientation = "N" | "E" | "W" | "S"
lazy val space = elem(' ')+

def integer  = """(0|[1-9]\d*)""".r ^^ { _.toInt }

/* parser Surface */
lazy val exprSurface = (integer <~ space) ~ integer ^^ { case width ~ height => new Surface(width,height)}  

/* parser Mower */
lazy val exprMower = (integer <~ space) ~ (integer <~ space) ~ orientation  ^^ { case x ~ y ~ oStr  => { 
	(x ,y ,oStr match {
	case "N" => Orientation.N
	case "E" => Orientation.E
	case "W" => Orientation.W
	case "S" => Orientation.S
	})
}} 

/* parser Mower commands*/
lazy val commande= "D" | "G" | "A"
lazy val exprCommandes = commande+

/* parser Mower and commands block)
 * parsing result is a function to apply on a Surface instance */ 
lazy val exprGoMower = (exprMower <~ separator) ~ exprCommandes ^^ { case expM ~ cmds => (s:Surface) => { 
	val t = new s.Mower(expM._1,expM._2,expM._3);
	cmds.foreach(s => { s match {
	case "D" => {t.turnRight}
	case "A" => {t.moveForward}
	case "G" => {t.turnLeft}
	}})
	t
}  ;
}

/* Global parser */
lazy val exprAll = (exprSurface <~ separator) ~ (((exprGoMower <~ separator)+) <~ eoi) ^^ { case s ~ expGM => expGM.map(f => f(s)) }

}