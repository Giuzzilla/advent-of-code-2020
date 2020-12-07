import scala.io.Source
import annotation.tailrec

case class Bag(name: String, amount: Int)

object Day7 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def findBags(str: String): List[Bag] = {
      val pattern = "(?:((\\d) ([a-z]+ [a-z]+)) bag(?:s)?[,\\.])+".r
      val it = pattern findAllIn str
      (for {
        matched <- it.matchData
        num = matched.group(2).toInt
        bagName = matched.group(3)
      } yield Bag(bagName, num)).toList
    }

    def parseLine(str: String): (String, List[Bag]) = {
      val splitted = str.split(" contain ")
      (splitted(0).split(" bag")(0), findBags(splitted(1)))
    }

    val list = Source.fromFile(path).getLines.toList.map(parseLine)

    def firstStar(lst: List[(String, List[Bag])], initialBag: String): Int = {
      @tailrec
      def accRecurse(
          currentBags: List[String],
          seen: Set[String]
      ): Set[String] = {
        val newBags = currentBags
          .map(bag =>
            lst
              .filter { case (_, tos) => tos.map(_.name).contains(bag) }
              .map(_._1)
              .toSet
          )
          .reduce(_ | _)
        if (newBags.size == 0)
          seen union newBags
        else
          accRecurse(newBags.toList, seen union newBags)
      }
      accRecurse(initialBag :: Nil, Set[String]()).size
    }

    def secondStar(lst: List[(String, List[Bag])], initialBag: String) = {
      @tailrec
      def nestSum(
          currPoint: String,
          multiplier: Int,
          accTot: Int,
          remaining: List[(Int, Bag)]
      ): Int = {
        val newList = lst.filter(_._1 == currPoint).head._2
        val toExplore = newList.map((multiplier, _)) ::: remaining
        if (toExplore.isEmpty)
          accTot
        else {
          val localTot = multiplier * newList.map(_.amount).sum
          val (nextMultiplier, nextBag) = toExplore.head
          nestSum(
            nextBag.name,
            nextMultiplier * nextBag.amount,
            accTot + localTot,
            toExplore.tail
          )
        }
      }
      nestSum(initialBag, 1, 0, List[(Int, Bag)]())
    }

    println(
      "First answer: " + firstStar(list, "shiny gold")
    )
    println(
      "Second answer: " + secondStar(list, "shiny gold")
    )
  }
}
