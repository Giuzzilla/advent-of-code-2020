import scala.io.Source
import scala.annotation.tailrec

case class Seat(id: String) {
  val conversionMap = Map(
    'F' -> '0',
    'B' -> '1',
    'L' -> '0',
    'R' -> '1'
  )

  val convertedId = id.toList.map(conversionMap).mkString("")

  val numericId = Integer.parseInt(convertedId, 2)
  val row: Int = Integer.parseInt(convertedId.take(7), 2)
  val col: Int = Integer.parseInt(convertedId.takeRight(3), 2)
}


object Day5 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val list: List[String] =
      Source.fromFile(path).getLines.toList

    val seats = list.map(Seat)

    val neededCol = (0 to 7).toSet
    val groupedSeats = seats.groupMap(_.row)(_.col)
    val idMissing = (for {
      (k, l) <- groupedSeats
      colDiff = neededCol diff l.toSet
      if colDiff.size == 1
    } yield k * 8 + colDiff.head).head

    println(
      "First answer: " + seats.map(_.numericId).max
    )
    println(
      "Second answer: " + idMissing
    )
  }
}
