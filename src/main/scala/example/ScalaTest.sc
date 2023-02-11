import scala.annotation.tailrec
import scala.io.Source

case class Coordinate(x: Int, y: Int)
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


def writeInCSV(funProg: FunProg) = {
  println("numero;debut_x;debut_y;debut_direction;fin_x;fin_y;fin_direction;instructions")
  val mowers = funProg.mowers
  for (i <- mowers.indices) {
    val mower = mowers(i)
    val finalPosition = mower.getFinalPosition(mower.position, funProg.limit, mower.instructions).head
    println(s"${i+1};${mower.position.point.x};${mower.position.point.y};${mower.position.direction};${finalPosition.point.x};${finalPosition.point.y};${finalPosition.direction};${mower.instructions.mkString}")
  }
}

writeInCSV(funProg)

/**
 * liste coord
 * liste action
 *
 *
 * double boucle (listes)
 *  > init Mower()
 *
 * Funprog() {
 *   limit
 *   Liste mower
 * }
 *
 *
 *
 *
 * ALGO
 * - Bordure en faisant attention à la direction
 *
 *
 * Classes :
 * Pour l'input (lecture fichier / génération Mowers)
 * Pour les objects :
 *  > Mower = faire l'algo pour la position finale
 * Pour l'output
 *
 *
 *
 * Directions
 * (N E S W)
 *  0 1 2 3
 *
 * G = -1
 * D = +1
 *
 * % 4
 *
 *
 * Pour avancer :
 * A
 * Si direction = N : x, y+1
 * Si direction = E : x+1, y
 * Si direction = S : x, y-1
 * Si direction = W : x-1, y
 * Si x < 0 ou y < 0 ou x > limit ou y > limit = bouge pas
 *
 *
 *
 * ETAPES
 * Input File => Read file
 * Creation FunProg w/ Objects initialization
 *
 * Output File
 *
 *
 */

