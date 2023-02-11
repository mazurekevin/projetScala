import scala.annotation.tailrec
import scala.io.Source


case class Position(point: Coordinate, direction: Char)
case class Mower(position: Position, instructions: List[Char]) {
  private val defaultDirections = List('N', 'E', 'S', 'W')
  private def _updateDirectionOfPosition(currentPosition: Position, instruction: Char): Position = {
    val currentDirection = currentPosition.direction
    val indexDirection = defaultDirections.indexOf(currentDirection)
    if (indexDirection >= 0) {
      if (instruction == 'G') {
        Position(currentPosition.point, defaultDirections(Math.floorMod(indexDirection - 1, defaultDirections.length)))
      } else if (instruction == 'D') {
        Position(currentPosition.point, defaultDirections(Math.floorMod(indexDirection + 1, defaultDirections.length)))
      } else {
        currentPosition
      }
    }
    else {
      currentPosition
    }
  }

  private def _updateCoordinateOfPosition(currentPosition: Position, limit: Coordinate): Position = {
    val currentPoint = currentPosition.point
    val currentDirection = currentPosition.direction
    currentDirection match {
      case 'N' => {
        val newPointY = currentPoint.y + 1
        if (newPointY >= 0 && newPointY <= limit.y) {
          Position(Coordinate(currentPoint.x, newPointY), currentDirection)
        }
        else {
          currentPosition
        }
      }
      case 'E' => {
        val newPointX = currentPoint.x + 1
        if (newPointX >= 0 && newPointX <= limit.x) {
          Position(Coordinate(newPointX, currentPoint.y), currentDirection)
        }
        else {
          currentPosition
        }
      }
      case 'S' => {
        val newPointY = currentPoint.y - 1
        if (newPointY >= 0 && newPointY <= limit.y) {
          Position(Coordinate(currentPoint.x, newPointY), currentDirection)
        }
        else {
          currentPosition
        }
      }
      case 'W' => {
        val newPointX = currentPoint.x - 1
        if (newPointX >= 0 && newPointX <= limit.x) {
          Position(Coordinate(newPointX, currentPoint.y), currentDirection)
        }
        else {
          currentPosition
        }
      }
    }
  }

  def getFinalPosition(currentPosition: Position, limit: Coordinate, instructions: List[Char]): List[Position] = instructions match {
    case head :: Nil => head match {
      case 'D' | 'G' => List(_updateDirectionOfPosition(currentPosition, head))
      case 'A' => List(_updateCoordinateOfPosition(currentPosition, limit))
    }
    case head :: tail => head match {
      case 'D' | 'G' => getFinalPosition(_updateDirectionOfPosition(currentPosition, head), limit, tail)
      case 'A' => getFinalPosition(_updateCoordinateOfPosition(currentPosition, limit), limit, tail)
    }
  }
}
case class FunProg(limit: Coordinate, mowers: List[Mower])

val fileName = "D:\\ESGI\\2022-2023 5AL\\Programmation fonctionnelle\\projetScala\\src\\main\\dataEnter.txt"
val lines = Source.fromFile(fileName).getLines
val values = lines.toList

def convertStringToCoordinate(inputPosition: String): Coordinate = {
  val values = inputPosition.split(" ")
  Coordinate(values.head.toInt, values(1).toInt)
}
def convertStringToPosition(inputPosition: String): Position = {
  val values = inputPosition.split(" ")
  Position(Coordinate(values.head.toInt, values(1).toInt), values(2).head)
}
def getMowersFromInput(values: List[String]): List[Mower] = values match {
  case coord :: action :: Nil => List(Mower(convertStringToPosition(coord), action.toList))
  case coord :: action :: tail => Mower(convertStringToPosition(coord), action.toList) :: getMowersFromInput(tail)
}

val limit = values.head
val tail = values.tail

val mowers = getMowersFromInput(tail)

val funProg = FunProg(convertStringToCoordinate(limit), mowers)

for (mower <- funProg.mowers) {
  println(mower)
  println(mower.getFinalPosition(mower.position, funProg.limit, mower.instructions))
}

sealed trait OutputType {
  def print()
  def write()
}
case class CsvOutput(funProg: FunProg) extends OutputType {

  override def print() = {
    println("numero;debut_x;debut_y;debut_direction;fin_x;fin_y;fin_direction;instructions")
    val mowers = funProg.mowers
    for (i <- mowers.indices) {
      val mower = mowers(i)
      val finalPosition = mower.getFinalPosition(mower.position, funProg.limit, mower.instructions).head
      println(s"${i + 1};${mower.position.point.x};${mower.position.point.y};${mower.position.direction};${finalPosition.point.x};${finalPosition.point.y};${finalPosition.direction};${mower.instructions.mkString}")
    }
  }
  override def write(): Unit = {

  }
}
case class JsonOutput(funProg: FunProg) extends OutputType {

  private def _displayLimit(): String = {
    s"""  "limite": {
       |      "x": ${funProg.limit.x},
       |      "y": ${funProg.limit.y}
       |    },
       |""".stripMargin
  }

  private def _displayCoordinate(position: Position): String = {
    s""""point": {
       |  "x": ${position.point.x},
       |  "y": ${position.point.y}
       |},
       |"direction": ${position.direction}""".stripMargin
  }

  private def _displayMower(mower: Mower): String = {
    val finalPosition = mower.getFinalPosition(mower.position, funProg.limit, mower.instructions).head
    s"""{
       |   "debut": {
       |     ${_displayCoordinate(mower.position)}
       |   },
       |   "instructions": "[${mower.instructions.mkString(",")}]",
       |   "fin" : {
       |     ${_displayCoordinate(finalPosition)}
       |   }
       |}""".stripMargin
  }
  override def print() = {
    val list_string_mowers = for (mower <- funProg.mowers) yield _displayMower(mower)
    println(
      s"""{
         |  ${_displayLimit()}
         |  "tondeuses": [
         |    ${list_string_mowers.mkString(",")}
         |  ]
         |}""".stripMargin)
  }
  override def write(): Unit = {

  }
}

val json = JsonOutput(funProg)
json.print()

/**
 *
 * ETAPES
 * Input File => Read file
 * Creation FunProg w/ Objects initialization
 *
 * Output File
 *
 *
 */

