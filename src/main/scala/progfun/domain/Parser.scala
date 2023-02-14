package progfun.domain

import scala.io.Source

case class Parser(input: Input) {
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

  private def _readFile() = {
    Source.fromFile(input.fileName).getLines().toList
  }

  /*
  private def _checkLimit(limit: String) = {
    val limitWithoutSpace = limit.replace(" ", "")
    limitWithoutSpace.length == 2 && limitWithoutSpace.forall(_.isDigit)
  }
   */

  def parseInput(): FunProg = {
    val values = _readFile()
    val limit = values.headOption.getOrElse("")
    val actions = values.drop(1)

    val mowers = getMowersFromInput(actions)
    FunProg(convertStringToCoordinate(limit), mowers)
  }

}
