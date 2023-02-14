package example

import org.scalatest.funsuite.AnyFunSuite
import progfun.domain.{Coordinate, Input, Parser, Position}

class MowerTest extends AnyFunSuite {
  val fileName = {
    "D:\\ESGI\\2022-2023 5AL\\Programmation fonctionnelle\\projetScala\\src\\main\\resources\\dataEnter.txt"
  }

  val input = Input(fileName)
  val parser = Parser(input)

  val funProg = parser.parseInput()
  val mowers = funProg.mowers

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
