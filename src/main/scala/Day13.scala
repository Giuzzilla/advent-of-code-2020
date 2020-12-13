import scala.io.Source
import scala.annotation.tailrec

object Day13 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def firstStar(lst: List[Int], initTime: Int): Int = {
      @tailrec
      def recFirst(time: Int): Int = {
        val result = (for {
          bus <- lst
          if (time % bus == 0)
        } yield (bus, time))

        if (!result.isEmpty) {
          val (bus, time) = result.head
          bus * (time - initTime)
        } else
          recFirst(time + 1)
      }
      recFirst(initTime)
    }

    @tailrec
    def secondStar(
        lst: List[(Int, Int)],
        next: BigInt = BigInt(0),
        step: BigInt = BigInt(1)
    ): BigInt = {
      val (matched, left) = lst.partition { case (id, idx) =>
        (next + idx) % id == 0
      }
      if (left.isEmpty)
        next
      else {
        val newStep = step * matched.map(_._1).foldLeft(BigInt(1))(_ * _)
        val newNext = next + newStep
        secondStar(left, newNext, newStep)
      }
    }

    val twoLines = Source.fromFile(path).getLines.toList
    val initTime = twoLines(0).toInt
    val list = twoLines(1)
      .split(",")
      .zipWithIndex
      .filter(_._1 != "x")
      .map { case (busId, idx) => (busId.toInt, idx) }
      .toList

    println(
      "First answer: " + firstStar(list.map(_._1), initTime)
    )
    println(
      "Second answer: " + secondStar(list)
    )
  }
}
