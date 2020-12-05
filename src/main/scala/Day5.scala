import scala.io.Source
import scala.annotation.tailrec

case class Seat(id: String) {
  @tailrec
  final def recPartition(
      startPos: Int,
      endPos: Int,
      signals: List[Boolean]
  ): Int =
    signals match {
      case left :: tail => {
        val newPos = startPos + (endPos - startPos) / 2
        if (left)
          recPartition(startPos, newPos, tail)
        else
          recPartition(newPos + 1, endPos, tail)
      }
      case Nil => startPos
    }

  val conversionMap = Map(
    'F' -> true,
    'B' -> false,
    'L' -> true,
    'R' -> false
  )

  val convertedId = id.toList.map(conversionMap)

  val row: Int = recPartition(0, 127, convertedId.take(7))
  val col: Int = recPartition(0, 7, convertedId.takeRight(3))

  val numericId = row * 8 + col
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
