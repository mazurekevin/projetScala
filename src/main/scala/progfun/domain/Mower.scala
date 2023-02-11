package progfun.domain

case class Mower(position: Position, instructions: List[Char]) {
  private val defaultDirections = List('N', 'E', 'S', 'W')
  private def _updateDirectionOfPosition(
      currentPosition: Position,
      instruction: Char
  ): Position = {
    val currentDirection = currentPosition.direction
    val indexDirection = defaultDirections.indexOf(currentDirection)
    if (indexDirection >= 0) {
      if (instruction == 'G') {
        Position(
          currentPosition.point,
          defaultDirections(
            Math.floorMod(indexDirection - 1, defaultDirections.length)
          )
        )
      } else if (instruction == 'D') {
        Position(
          currentPosition.point,
          defaultDirections(
            Math.floorMod(indexDirection + 1, defaultDirections.length)
          )
        )
      } else {
        currentPosition
      }
    } else {
      currentPosition
    }
  }

  private def _updateCoordinateOfPosition(
      currentPosition: Position,
      limit: Coordinate
  ): Position = {
    val currentPoint = currentPosition.point
    val currentDirection = currentPosition.direction
    currentDirection match {
      case 'N' => {
        val newPointY = currentPoint.y + 1
        if (newPointY >= 0 && newPointY <= limit.y) {
          Position(Coordinate(currentPoint.x, newPointY), currentDirection)
        } else {
          currentPosition
        }
      }
      case 'E' => {
        val newPointX = currentPoint.x + 1
        if (newPointX >= 0 && newPointX <= limit.x) {
          Position(Coordinate(newPointX, currentPoint.y), currentDirection)
        } else {
          currentPosition
        }
      }
      case 'S' => {
        val newPointY = currentPoint.y - 1
        if (newPointY >= 0 && newPointY <= limit.y) {
          Position(Coordinate(currentPoint.x, newPointY), currentDirection)
        } else {
          currentPosition
        }
      }
      case 'W' => {
        val newPointX = currentPoint.x - 1
        if (newPointX >= 0 && newPointX <= limit.x) {
          Position(Coordinate(newPointX, currentPoint.y), currentDirection)
        } else {
          currentPosition
        }
      }
    }
  }

  def getFinalPosition(
      currentPosition: Position,
      limit: Coordinate,
      instructions: List[Char]
  ): List[Position] = instructions match {
    case Nil => List()
    case head :: Nil =>
      head match {
        case 'D' | 'G' =>
          List(_updateDirectionOfPosition(currentPosition, head))
        case 'A' => List(_updateCoordinateOfPosition(currentPosition, limit))
      }
    case head :: tail =>
      head match {
        case 'D' | 'G' =>
          getFinalPosition(
            _updateDirectionOfPosition(currentPosition, head),
            limit,
            tail
          )
        case 'A' =>
          getFinalPosition(
            _updateCoordinateOfPosition(currentPosition, limit),
            limit,
            tail
          )
      }
  }
}
