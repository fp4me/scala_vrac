package mowit

import scala.io.Source

object Main extends App {

/*parsing du scenario définissant la surface , les tondeuses et leurs mouvements */  
val parser = new Parser 
val src = Source.fromFile(getClass.getResource("/scenario.txt").getPath).getLines.mkString("\n");

/* print final positions of mowers */  
parser.parse(parser.exprAll,src).getOrElse(List()).foreach(println)

}
