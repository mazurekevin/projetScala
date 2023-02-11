package progfun.domain.output

import progfun.domain.{Coordinate, FunProg, Position}

case class CsvOutput(funProg: FunProg) extends Output {

  override def print() = {
    println(
      "numero;debut_x;debut_y;debut_direction;fin_x;fin_y;fin_direction;instructions"
    )
    val mowers = funProg.mowers
    for (i <- mowers.indices) {
      val mower = mowers(i)
      val finalPosition = mower
        .getFinalPosition(mower.position, funProg.limit, mower.instructions)
        .headOption
        .getOrElse(Position(Coordinate(0, 0), 'N'))
      println(
        s"${(i + 1).toString};${mower.position.point.x.toString};${mower.position.point.y.toString};${mower.position.direction.toString};" +
          s"${finalPosition.point.x.toString};${finalPosition.point.y.toString};${finalPosition.direction.toString};" +
          s"${mower.instructions.mkString}"
      )
    }
  }
  override def write(): Unit = {}
}
