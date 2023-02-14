package fr.esgi.al.funprog

import progfun.domain.{Input, Parser}
import progfun.domain.output.{CsvOutput, JsonOutput}

object Main extends App {

  val fileName = {
    "D:\\ESGI\\2022-2023 5AL\\Programmation fonctionnelle\\projetScala\\src\\main\\resources\\dataEnter.txt"
  }

  val input = Input(fileName)
  val parser = Parser(input)

  val funProg = parser.parseInput()

  val json = JsonOutput(funProg)
  val csv = CsvOutput(funProg)

  json.print()
  csv.print()

}
