package mowit


/* class (rectangular) Surface defined by its max values for coordinates */
class Surface(xMax: Int, yMax : Int)  {
 
  /* innerClass Mower defined by its coordinates and orientation on the Surface */ 
  class Mower(private var posX: Int, 
              private var posY:Int,
              private var posO:Orientation.Value) {

   override def toString = { posX + " " + posY + " " + posO }
   
   def turnLeft = { posO = Orientation((posO.id-1+4) % 4)  }
   def turnRight = { posO = Orientation((posO.id+1) % 4)  }
   def moveForward = { posO match {
     case Orientation.N => { posY = math.min (posY + 1,Surface.this.yMax)}
     case Orientation.W => { posX = math.max (posX - 1, 0)}
     case Orientation.S => { posY = math.max (posY - 1, 0)}
     case Orientation.E => { posX = math.min (posX + 1,Surface.this.xMax)} 
     }
   } 
   }
}