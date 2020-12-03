import scala.io.Source
import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    case class Slope(right: Int, down: Int)
    def countTraverse[A](toFind: A)(matrix: Vector[Vector[A]], slope: Slope): Int = {
      @tailrec def accTraverse(row: Int, column: Int, count: Int): Int = {
        if (row >= matrix.length)
          count
        else {
          val currentRow = matrix(row)
          val newCount = if (currentRow(column % currentRow.length) == toFind) count + 1 else count
          accTraverse(row + slope.down, column + slope.right, newCount)
        }
      }
      accTraverse(0, 0, 0)
    }

    val mat: Vector[Vector[Char]] = Source.fromFile(path).getLines.toVector.map(_.toVector)
    def solve = countTraverse[Char]('#')(_, _)

    println(
      "First answer: " + solve(mat, Slope(3, 1))
    )

    val slopes = List(
      Slope(1, 1),
      Slope(3, 1),
      Slope(5, 1),
      Slope(7, 1),
      Slope(1, 2)
    )
    val counts: List[BigInt] = slopes.map(solve(mat, _))

    println(
      "Second answer: " + counts.product
    )
  }
}