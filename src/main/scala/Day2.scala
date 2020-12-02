import scala.io.Source

case class Password(num1: Int, num2: Int, letter: Char, password: String) {
  def isValid: Boolean = {
    val count = password.count(_ == letter)
    count >= num1 && count <= num2
  }

  def isValid2: Boolean = {
    val first: Boolean = password(num1 - 1) == letter
    val second: Boolean = password(num2 - 1) == letter
    first != second
  }
}

object Day2 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val pattern = "(\\d+)-(\\d+) ([A-Za-z]): ([A-Za-z]+)".r
    def parseLine(input: String): Password = input match {
      case pattern(num1, num2, letterStr, password) =>
        Password(num1.toInt, num2.toInt, letterStr(0), password)
    }

    val list: List[Password] = Source.fromFile(path).getLines.toList.map(parseLine)

    println(
      "First answer: " + list.filter(_.isValid).length
    )
    println(
      "Second answer: " + list.filter(_.isValid2).length
    )
  }
}

