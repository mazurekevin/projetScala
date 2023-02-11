package progfun.domain.output

import progfun.domain.{Coordinate, FunProg, Mower, Position}

case class JsonOutput(funProg: FunProg) extends Output {

  private def _displayLimit(): String = {
    s"""  "limite": {
       |      "x": ${funProg.limit.x.toString},
       |      "y": ${funProg.limit.y.toString}
       |    },
       |""".stripMargin
  }

  private def _displayCoordinate(position: Position): String = {
    s""""point": {
       |  "x": ${position.point.x.toString},
       |  "y": ${position.point.y.toString}
       |},
       |"direction": ${position.direction.toString}""".stripMargin
  }

  private def _displayMower(mower: Mower): String = {
    val finalPosition = mower
      .getFinalPosition(mower.position, funProg.limit, mower.instructions)
      .headOption
      .getOrElse(Position(Coordinate(0, 0), 'N'))
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
    val list_string_mowers =
      for (mower <- funProg.mowers) yield _displayMower(mower)
    println(s"""{
         |  ${_displayLimit()}
         |  "tondeuses": [
         |    ${list_string_mowers.mkString(",")}
         |  ]
         |}""".stripMargin)
  }
  override def write(): Unit = {}
}
