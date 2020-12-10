import scala.io.Source
import annotation.tailrec

object Day10 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val list = Source.fromFile(path).getLines.map(_.toInt).toList
    val newList = (0 :: list.max + 3 :: list)


    def get_children(lst: List[Int], value: Int): List[Int] =
      lst.filter(el => el > value && el <= value + 3)

    def get_parents(lst: List[Int], value: Int): List[Int] =
      lst.filter(el => el < value && el >= value - 3)


    def countSteps(lst: List[Int]): Int = {
      @tailrec
      def recSteps(curr: Int, count1: Int, count3: Int): (Int, Int) = {
        val children = get_children(lst, curr)
        if (children.isEmpty)
          (count1, count3)
        else {
          val newCurr = children.min
          val (newCount1: Int, newCount3: Int) =
            if (newCurr - curr == 1)
              (count1 + 1, count3)
            else if (newCurr - curr == 3)
              (count1, count3 + 1)
          recSteps(newCurr, newCount1, newCount3)
        }
      }
      val (c1, c3) = recSteps(0, 0, 0)
      c1 * c3
    }

    def countPaths(lst: List[Int]): BigInt = {
      val initWays: Map[Int, BigInt] =
        lst.map(_ -> BigInt(0)).toMap.updated(lst.max, BigInt(1))
      @tailrec
      def explorePaths(
          currs: List[Int],
          ways: Map[Int, BigInt]
      ): Map[Int, BigInt] = {
        val newCurrs = (for {
          curr <- currs
          parent <- get_parents(lst, curr).take(3)
          childOfParent <- get_children(lst, parent).take(3)
        } yield (childOfParent, parent)).groupBy(_._2).mapValues(_.map(_._1)).toList
        if (newCurrs.length == 0)
          ways
        else {
          val newWays = newCurrs
            .sortBy(-_._1)
            .foldLeft(ways)((w, el) => w.updated(el._1, el._2.map(w(_)).sum))
          explorePaths(newCurrs.map(_._1).distinct, newWays)
        }
      }
      explorePaths(List(lst.max), initWays)(0)
    }

    println(
      "First answer: " + countSteps(newList)
    )
    println(
      "Second answer: " + countPaths(newList)
    )
  }
}
