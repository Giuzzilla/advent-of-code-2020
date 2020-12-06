import scala.io.Source

object Day6 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val groups: List[String] =
      Source.fromFile(path).getLines.mkString("\n").split("\n\n").toList

    println(
      "First answer: " + groups.map(_.replace("\n", "").toSet.size).sum
    )
    println(
      "Second answer: " + groups
        .map(_.split("\n").map(_.toSet).reduce(_ & _).size)
        .sum
    )
  }
}
