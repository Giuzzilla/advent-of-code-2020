import scala.io.Source
import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]) {
    val input: String = {
      if (args.length == 0)
        throw new IllegalArgumentException("Must provide an input for Day 15")
      else
        args(0)
    }

    val list = input.split(",").map(_.toInt).toList

    def solution(lst: List[Int], steps: BigInt): BigInt = {
      @tailrec
      def recFunc(
          memory: Map[BigInt, BigInt],
          last: BigInt,
          idx: BigInt
      ): BigInt = {
        if (idx == steps - 1)
          last
        else {
          val nextVal =
            if (memory.contains(last))
              idx - memory(last)
            else
              BigInt(0)

          recFunc(memory + (last -> idx), nextVal, idx + 1)
        }
      }
      recFunc(
        lst.zipWithIndex.map(t => (BigInt(t._1), BigInt(t._2))).toMap,
        BigInt(lst.last),
        BigInt(lst.length - 1)
      )
    }

    println(
      "First answer: " + solution(list, 2020)
    )
    println(
      "Second answer: " + solution(list, 30000000)
    )
  }
}
