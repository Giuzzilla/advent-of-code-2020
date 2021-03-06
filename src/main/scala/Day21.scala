import scala.io.Source
import scala.annotation.tailrec

case class Item(ingredients: List[String], allergens: List[String])

object Day21 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def parseItem(s: String): Item = {
      val splitted = s.split(" \\(contains ")
      val ingredients = splitted(0).split(" ").toList
      val allergens =
        if (splitted.length > 1)
          splitted(1).slice(0, splitted(1).length - 1).split(", ").toList
        else
          List[String]()

      Item(ingredients, allergens)
    }

    @tailrec
    def recEliminate(
        currMap: Map[String, Set[String]],
        lst: List[(String, String)]
    ): List[(String, String)] = {
      if (currMap.isEmpty)
        lst
      else {
        val singlesMap = currMap.filter { case (_, v) => v.size == 1 }
        val singles = singlesMap.map { case (k, v) => (k, v.head) }.toList
        val newMap = currMap.map { case (k, v) =>
          k -> v.filter(a => !singles.map(_._2).contains(a))
        } -- singlesMap.keys
        val newLst = singles ::: lst
        recEliminate(newMap, newLst)
      }
    }

    val list = Source.fromFile(path).getLines.toList.map(parseItem(_))

    val allergenMap = (for {
      food <- list
      allergen <- food.allergens
      ingred = food.ingredients
    } yield (allergen, ingred)).groupBy(_._1).toMap.map { case (k, v) =>
      k -> v.map(_._2.toSet).reduce(_ & _)
    }

    val goodList =
      (list.flatMap(_.ingredients).toSet diff allergenMap.map(_._2).reduce(_ | _))

    println(
      "First answer: " + list
        .flatMap(_.ingredients)
        .filter(goodList.contains(_))
        .length
    )
    println(
      "Second answer: " + recEliminate(allergenMap, List[(String, String)]())
        .sortBy(_._1)
        .map(_._2)
        .mkString(",")
    )
  }
}
