package mowit

import org.scalatest._

class MowitSpec extends FlatSpec with Matchers with GivenWhenThen {
  "The Mower" should "move correctly" in {
    Given("a new Mower with position 1 1 N in a surface with maxX=1 and maxY=1 ")
    val s = new Surface(1,1)
    val mower = new s.Mower(1,1,Orientation.N)
    
    When ("mower move forward but reach upperbound y limit of surface")
    mower.moveForward
    Then("new position still 1 1 N")
    assert(mower.toString == "1 1 N")
    
    When ("mower turn left")
    mower.turnLeft
    Then("new position should be 1 1 W")
    assert(mower.toString == "1 1 W")
    
    When ("mower turn left again.")
    mower.turnLeft
    Then("new position should be 1 1 S")
    assert(mower.toString == "1 1 S")
    
     When ("mower move forward.")
    mower.moveForward
    Then("new position should be 1 0 S")
    assert(mower.toString == "1 0 S")
    
    When ("mower turn right")
    mower.turnRight
    Then("new position should be 1 0 W")
    assert(mower.toString == "1 0 W")

    When ("mower move forward but reach x lowerbound limit.")
    mower.moveForward
    Then("new position still  0 0 W")
    assert(mower.toString == "0 0 W")
    
    When ("mower turn right again.")
    mower.turnRight
    Then("new position should be 0 0 N")
    assert(mower.toString == "0 0 N") 
  }
  
  "Parsing scenario" should "return result expected" in {
   val scenario = """5 5
     |1 2 N
     |GAGAGAGAA
     |3 3 E
     |AADAADADDA""".stripMargin
   val parser = new Parser
   val mowers = parser.parse(parser.exprAll,scenario).getOrElse(List())
   mowers.map(x =>x.toString) should be (List("1 3 N","5 1 E"))
  }
}
