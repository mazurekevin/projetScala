import scala.io.Source
case class FunProg(limit: String, mowers:  List[Mower])
case class Mower(position: String, actions: String)

val fileName = "C:\\Users\\kevin\\Downloads\\template de projet Scala_SBT\\projet\\funprog-al\\src\\main\\dataEnter.txt"
val mower = Mower("","")
val funProg = FunProg("",List(mower))
val lines = Source.fromFile(fileName).getLines
val values = lines.toList
def f(index: Int, line: String, funProg: FunProg): String = index match {
  case 0 => "limit "+ line
  case index => if(index % 2 != 0)"impaire "+ line else "pair "+ line
}
for(line <- values){

  val index = values.indexOf(line)
  println(f(index,line,funProg))
}





