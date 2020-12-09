import scala.io.Source
import annotation.tailrec

object Day9 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val vect = Source.fromFile(path).getLines.map(BigInt(_)).toVector
    val idxVect = vect.zipWithIndex.slice(25, vect.length)

    val missingIdx = (idxVect.map(_._2).toSet diff (for {
      (num, idx) <- idxVect
      previous = vect.slice(idx-25, idx)
      prev1 <- previous
      prev2 <- previous
      if (prev1 != prev2 && prev1 + prev2 == num)
    } yield idx).toSet).head
    val firstStar = vect(missingIdx)


    def contPosSublistSum(list: List[BigInt], target: BigInt): List[BigInt] = {
      val leftSum: List[(BigInt, Int)] = list.scanLeft(BigInt(0))(_ + _).zipWithIndex
      leftSum.foldLeft((leftSum, List[(Int, Int)]())) {
          case ((ls, matches), currTot) =>
            val newLSum = ls.dropWhile(currTot._1 - _._1 > target)
            if (currTot._1 - newLSum.head._1 == target)
              (newLSum, (newLSum.head._2, currTot._2) :: matches)
            else (newLSum, matches)
      }._2.filter { case (a, b) => b - a > 1}.map{ case (a, b) => list.slice(a, b)}(0)
    }

    val matchedSeq = contPosSublistSum(vect.toList, firstStar)

    println(
      "First answer: " + firstStar
    )
    println(
      "Second answer: " + (matchedSeq.min + matchedSeq.max)
    )
  }
}
