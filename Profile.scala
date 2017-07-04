import scala.xml._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.actors.Actor
import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import java.io.FileWrite

class Profile () {
} 

object Profiles extends RegexParsers {

override def skipWhitespace = false

def parseFile(i:Iterator[String]):List[Rule] = { 

var profiles:List[String] = Nil
i.foldLeft(profiles,"")(itFile)

}




def loadProfiles(fromFile:String):List[Profile] = {
 val profiles = parseFile(scala.io.Source.fromFile(configFile).getLines)
 profiles
}

lazy val space = (elem(' ') | elem('\t') | elem('\r')).+
lazy val plusOrMinus = elem('-') | elem('+')

lazy val allExcept2p = regex(new Regex("^[:]+"))
lazy val typePermission = ("Y"|"N"|"+"|"-")

val anyValue   = regex(new Regex(".+"))

lazy lineProfile = "Profile = " ~> anyValue
lazy lineApplication = "Application = " ~> anyValue
lazy lineFunction = opt(space) ~> "Function = " ~> anyValue
lazy lineEntetePermissions = opt(space) ~ "Permissions ="
lazy linePermission = space ~>  ((allExcept2p <~ ":") ~ typePermission  )  
lazy linePermissionSuite = space ~> allExcept2p 


}
