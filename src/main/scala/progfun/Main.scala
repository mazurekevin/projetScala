package fr.esgi.al.funprog

import progfun.domain.FunProg
import progfun.domain.Input
import progfun.domain.output.{CsvOutput, JsonOutput}

import scala.io.Source

object Main extends App {

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

  val json = JsonOutput(funProg)
  val csv = CsvOutput(funProg)

  json.print()
  csv.print()

}
