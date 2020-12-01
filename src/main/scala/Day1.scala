import scala.io.Source

object Day1 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val list: List[Int] = Source.fromFile(path).getLines.toList.map(_.toInt)
    def solve(lst: List[Int], nCombinations: Int): Int =
      lst.combinations(nCombinations)
        .filter { _.sum == 2020 }
        .next
        .product

    println(
      "First answer: " + solve(list, 2)
    )
    println(
      "Second answer: " + solve(list, 3)
    )
  }
}
