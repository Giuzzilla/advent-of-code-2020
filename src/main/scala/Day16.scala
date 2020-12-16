import scala.io.Source
import scala.annotation.tailrec


case class Range(start: Int, end: Int) {
  def fits(num: Int) =
    num >= start && num <= end
}

case class Rule(name: String, ranges: List[Range]) {
  def valid(num: Int): Boolean =
    ranges.exists(_.fits(num))
}

object Day16 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def parseRule(s: String): Rule = {
      val nameRange = s.split(": ")
      val name = nameRange(0)
      val strRanges = nameRange(1).split(" or ")
      val ranges = strRanges.map(s => {
        val splitted = s.split("-")
        Range(splitted(0).toInt, splitted(1).toInt)
      })
      Rule(name, ranges.toList)
    }

    def parseTicket(l: String): List[Int] =
      l.split(",").map(_.toInt).toList

    val list = Source.fromFile(path).getLines.toList
  
    val rules = list.slice(0, 20).map(parseRule)
    val allRanges = rules.flatMap(_.ranges)

    val myTicket = parseTicket(list(22)).map(BigInt(_))
    val allTickets = list.slice(25, list.length).map(parseTicket)

    val (validTickets, invalidTickets) = allTickets.partition(
      ticket => ticket.forall(value => allRanges.exists(_.fits(value)))
    )

    val invalidValues = invalidTickets.flatMap(lst =>
      lst.filter(value => !allRanges.exists(_.fits(value)))
    )

    val ids2Rules = (for {
      ticket <- validTickets
      (value, idx) <- ticket.zipWithIndex
      rule <- rules
      if (rule.valid(value))
    } yield (idx, rule.name)).toList.groupBy(_._1).map { case (k, v) => k->v.map(_._2).groupBy(identity).filter(_._2.size == validTickets.length).map(_._1).toList}


    @tailrec
    def recEliminate(matches: Map[Int, List[String]], accMat: List[(Int, String)]): Map[Int, String] = {
      if (matches.forall(_._2.length <= 1)) {
        accMat.toMap
      } else {
        val found = matches.filter(t => t._2.length == 1).map(t => (t._1, t._2(0))).toList
        val newMat = matches.map(t => (t._1, t._2.filter(!found.map(_._2).contains(_))))
        recEliminate(newMat, found ::: accMat)
      }
    }

    val associations = recEliminate(ids2Rules, List[(Int, String)]())

    val depRulesIds = associations.filter(_._2.startsWith("departure")).map(_._1)


    println(
      "First answer: " + invalidValues.sum
    )
    println(
      "Second answer: " + depRulesIds.foldLeft(BigInt(1))((acc, idx) => {
        acc * myTicket(idx)
      })
    )
  }
}
