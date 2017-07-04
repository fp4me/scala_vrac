
trait GetFromCsv {


 case class ~[+a, +b](_1: a, _2: b) {
     override def toString = "("+ _1 +"~"+ _2 +")"  

    def ~[U](field:U) = this match {
                             case x ~ (y:Option[Any]) if y==None => new ~(x,field)  
		             case _ => new ~(this,field)
                            }
 }

 class Champ[A](x:A) {

    def value = x
    protected var next:Option[Champ[A]] = None
    def ~(champ:A):Champ[A] = { next match {
                        case None => next = Some(new Champ(champ))
                        case Some(f) => f ~ champ
                       }
                       this
                     }    

    override  def toString =  "[" + value + "] " + next.getOrElse("") 

    override def equals(that:Any) :Boolean = that match {
      case other:Champ[A] => ((value == other.value) && (next == other.next))
      case _ => false
     }

    override def hashCode = value.hashCode + next.hashCode   

    def map[B](f: A => B):Champ[B]  = next match {
                                                  case None => new Champ(f(value)) 
                                                  case Some(c:Champ[A]) =>  { val n = new Champ(f(value));  
                                                                              n.next =  Some(c.map(f)) ; 
                                                                              n
                                                                             } 
                                                 } 
      
    def toType:Any =  next match {
                                  case Some(s) => new ~(value,s.toType)
                                  case None => value
                                 } 


    def forAll(f:A => Boolean):Boolean = f(value) match {
                                                   case false => false
                                                   case true => next.map(_.forAll(f)).getOrElse(true)
                                                  } 
   }
 
 
 implicit def wrapChamp(s:String) = new Champ(s)
 

 private var actions = Map[ Champ[String] , Any => Unit]()

 private var header = Array[String]()

 def addAction (fields: Champ[String])(code: Any => Unit) = actions = actions + (fields -> code) 

 def getFields(line:String):Array[String] 
 def getHeader = getFields _

 def printActions = for { action <- actions.toList } { println(action._1) } 
 
 def computeFile(file:String) = {
   
   val itFile = scala.io.Source.fromFile(file).getLines
   header = getHeader(itFile.next)

   def getIndexFromHeader(keyword:String) = {
      val i = header.indexWhere(_==keyword)
      if (i<0) print ("header field["+keyword+"] not found \n");   
      i
   }

   val possiblesActions = actions.map(x => (x._1.map(getIndexFromHeader(_)),x._2)).filterKeys( _.forAll(_>=0)).toList

   for { lineText <- itFile } 
     {      
      
      val line = getFields(lineText)
      for { action <- possiblesActions }  if (line.length==header.length) action._2(action._1.map(line(_)).toType)
     }

}

} 


object GetText extends GetFromCsv {
 
  def getFields(line:String) = line.split("\",").map(_.drop(1))

  class Text(x:String,y:String) {
    override def toString = x + " : " + y
  }

  private var texts:List[Text] = Nil  

  addAction("text_s_umid" ~ ":E:text_data_block") { 
                           case (x:String) ~ (y:String)   => texts = new Text(x,y.take(40)) :: texts 
			   case _ => println("error")
                          } 

  addAction("text_s_umid" ) { 
                           case (x:String) => println(x) 
			   case _ => println("error")
                          } 

  def getList = texts

  //private var name: String = ""
  //def getType(s:String):this.type = {name=s;this}
  //override def toString() = "GetFromCsv ("+ name +")"

 }

