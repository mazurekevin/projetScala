package example

import org.scalatest.funsuite.AnyFunSuite
import progfun.domain.{Coordinate, FunProg, Input, Position}

import scala.io.Source

class MowerTest extends AnyFunSuite {
  val fileName =
    "C:\\Users\\kevin\\Downloads\\template de projet Scala_SBT\\projet\\funprog-al\\src\\main\\dataEnter.txt"
  val lines = Source.fromFile(fileName).getLines()
  val values = lines.toList

  val limit = values.headOption.getOrElse("")
  val actions = values.drop(1)

  val input = Input()
  val mowers = input.getMowersFromInput(actions)

  val funProg =
    FunProg(input.convertStringToCoordinate(limit), mowers)

  test("Test getFinalPosition") {
    val resultPosition = funProg
      .mowers(0)
      .getFinalPosition(
        mowers(0).position,
        funProg.limit,
        mowers(0).instructions
      )
    assert(resultPosition(0).direction === 'N')
    assert(resultPosition(0).point.x === 1)
    assert(resultPosition(0).point.y === 3)
  }

  test("Test updateCoordinateOfPosition") {
    val resultPosition = funProg
      .mowers(0)
      .updateCoordinateOfPosition(mowers(0).position, funProg.limit)
    assert(resultPosition.point.y === 3)
    assert(resultPosition.point.x === 1)

    val positionTest = Position(Coordinate(0, 0), 'S')
    val resultPosition2 =
      funProg.mowers(0).updateCoordinateOfPosition(positionTest, funProg.limit)
    assert(resultPosition2.point.y === 0)
    assert(resultPosition2.point.x === 0)
  }

  test("Test updateDirectionOfPosition") {
    val resultPosition =
      funProg.mowers(0).updateDirectionOfPosition(mowers(0).position, 'G')
    assert(resultPosition.direction === 'W')

    val resultPosition2 =
      funProg.mowers(0).updateDirectionOfPosition(mowers(0).position, 'P')
    assert(resultPosition2.direction === 'N')
  }

}
