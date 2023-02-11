package progfun.domain

case class Input() {
  def convertStringToCoordinate(inputPosition: String): Coordinate = {
    val values = inputPosition.split(" ")
    Coordinate(values.head.toInt, values(1).toInt)
  }

  def convertStringToPosition(inputPosition: String): Position = {
    val values = inputPosition.split(" ")
    Position(Coordinate(values.head.toInt, values(1).toInt), values(2).head)
  }

  def getMowersFromInput(values: List[String]): List[Mower] = values match {
    case cord :: action :: Nil =>
      List(Mower(convertStringToPosition(cord), action.toList))
    case cord :: action :: tail =>
      Mower(convertStringToPosition(cord), action.toList) :: getMowersFromInput(
        tail
      )
    case _ => List()
  }

}
