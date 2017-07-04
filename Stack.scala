object Stack {
  def sum(a: Int)(implicit b: Int): Int = { a+b } 
}

object Impl {
import Stack.sum
val j = sum(3)(4)
}
