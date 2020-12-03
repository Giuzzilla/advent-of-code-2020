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

    def countTraverse[A](toFind: A)(matrix: Vector[Vector[A]], right: Int, down: Int): Int = {
      @tailrec def accTraverse(row: Int, column: Int, count: Int): Int = {
        if (row >= matrix.length)
          count
        else {
          val currentRow = matrix(row)
          val newCount = if (currentRow(column % currentRow.length) == toFind) count + 1 else count
          accTraverse(row + down, column + right, newCount)
        }
      }
      accTraverse(0, 0, 0)
    }

    val mat: Vector[Vector[Char]] = Source.fromFile(path).getLines.toVector.map(_.toVector)
    def solve = countTraverse[Char]('#')(_, _, _)

    println(
      "First answer: " + solve(mat, 3, 1)
    )

    val possibleSlopes = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )
    val counts: List[BigInt] = possibleSlopes.map(slope => solve(mat, slope._1, slope._2))

    println(
      "Second answer: " + counts.product
    )
  }
}