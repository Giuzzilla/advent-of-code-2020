import scala.annotation.tailrec
import scala.io.Source

object Day25 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val pks = Source.fromFile(path).getLines.toList.map(_.toInt)

    def calc(
        maxLoop: Int,
        subj: Int,
        key: Int,
        accValue: BigInt = BigInt(1),
        iter: Int = 0
    ): (Int, BigInt) = {
      if (accValue == key || iter == maxLoop)
        (iter, accValue)
      else {
        calc(maxLoop, subj, key, (accValue * subj) % 20201227, iter + 1)
      }
    }

    val (loop1, _) = calc(Int.MaxValue, 7, pks(0))
    val (loop2, _) = calc(Int.MaxValue, 7, pks(1))

    val (_, secretKey1) = calc(loop1, pks(1), -1)
    val (_, secretKey2) = calc(loop2, pks(0), -1)

    assert(secretKey1 == secretKey2)

    println(
      "Answer: " + secretKey1
    )
  }
}
