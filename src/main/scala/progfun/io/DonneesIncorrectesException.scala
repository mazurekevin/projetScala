package progfun.io

class DonneesIncorrectesException(error: String) extends Exception {
  println(s"DonneesIncorrectesException: $error")
}
