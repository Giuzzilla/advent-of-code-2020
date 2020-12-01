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
    println("First answer: " + list.combinations(2).toList.view.filter{ case List(a, b) => a + b == 2020 }.head.product)
    println("Second answer: " + list.combinations(3).toList.view.filter{ case List(a, b, c) => a + b + c == 2020 }.head.product)
  }
}
