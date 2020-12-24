import scala.annotation.tailrec
import scala.io.Source


object Day24 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    type Tile = (Int, Int)

    def parseTile(s: String): Tile = {
      val patt = "(se|sw|nw|ne|w|e|s|n)".r
      val stepsLst = for {
        matched <- patt.findAllMatchIn(s)
        pos = matched.group(1) match {
          case "sw" => (-1, -1)
          case "se" => (-1, 1)
          case "nw" => (1, -1)
          case "ne" => (1, 1)
          case "e" => (0, 2)
          case "w" => (0, -2)
          case "n" => (2, 0)
          case "s" => (-2, 0)
        }
      } yield pos
      val finalPos = stepsLst.foldLeft((0,0))(
        (acc, step) => 
          (acc._1 + step._1, acc._2 + step._2)
      )
      finalPos
    }

    val list = Source.fromFile(path).getLines.toList.map(parseTile(_))

    val initBlacks = list.groupBy(identity).map(t => t._1->t._2.size).filter(_._2 % 2 != 0).keys.toSet

    def generateAdjacent(tile: Tile): List[Tile] = {
      val steps = List(
          (-1, -1),
          (-1, 1),
          (1, -1),
          (1, 1),
          (0, 2),
          (0, -2)
      )
      steps.map(s => (s._1 + tile._1, s._2 + tile._2))
    }

    @tailrec
    def solve2(blacks: Set[Tile], maxIters: Int = 100, iter: Int = 0): Set[Tile] = {
      if (iter == maxIters)
        blacks
      else {
        val allTiles = (for {
          tile <- blacks.toList
          adj <- generateAdjacent(tile)
        } yield adj).distinct
        val newBlacks = (for {
          tile <- allTiles
          adjs = generateAdjacent(tile).filter(blacks.contains(_))
        } yield (tile, adjs.length)).filter{
          case (k, v) =>
            if (blacks.contains(k))
              !(v == 0 || v > 2)
            else
              v == 2
        }.toList.map(_._1).toSet
        solve2(newBlacks, maxIters, iter + 1)
      }
    }


    println(
      "First answer: " + initBlacks.size
    )
    println(
      "Second answer: " + solve2(initBlacks).size
    )
  }
}