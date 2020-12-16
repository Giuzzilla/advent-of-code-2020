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

    def solution(lst: List[Int], steps: Long): Long = {
      @tailrec
      def recFunc(
          memory: Map[Int, Long],
          last: Int,
          idx: Long
      ): Long = {
        if (idx == steps - 1)
          last
        else {
          val nextVal =
            if (memory.contains(last))
              (idx - memory(last)).toInt
            else
              0

          recFunc(memory + (last -> idx), nextVal, idx + 1)
        }
      }
      recFunc(
        lst.zipWithIndex.map(t => (t._1, t._2.toLong)).toMap,
        lst.last,
        (lst.length - 1).toLong
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
