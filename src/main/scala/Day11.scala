import scala.io.Source
import annotation.tailrec

object Day11 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val vect = Source.fromFile(path).getLines.map(_.toVector).toVector

    def updatePos(name: Char, neighbors: List[Char], maxNeighbors: Int): Char =
      name match {
        case '.' => '.'
        case 'L' =>
          if (neighbors.forall(_ != '#')) '#' else 'L'
        case '#' =>
          if (neighbors.filter(_ == '#').length >= maxNeighbors) 'L' else '#'
      }

    def neighbors(
        vect: Vector[Vector[Char]],
        i: Int,
        j: Int,
        maxSight: Int = 1
    ): List[Char] = {
      val dirs = (for {
        dirX <- -1 to 1
        dirY <- -1 to 1
        if (dirX != 0 || dirY != 0)
      } yield (dirX, dirY)).toSet
      @tailrec
      def neighborsRec(
          neighbors: List[Char],
          sight: Int,
          unseenDirs: Set[(Int, Int)]
      ): List[Char] = {
        if (unseenDirs.isEmpty || sight > maxSight)
          neighbors
        else {
          val results = (for {
            (dirX, dirY) <- unseenDirs
            iNew = i + dirX * sight
            jNew = j + dirY * sight
            element =
              if (
                iNew >= 0
                && jNew >= 0
                && iNew < vect.length
                && jNew < vect(i).length
              ) vect(iNew)(jNew)
              else 'o'
            if (element != '.')
          } yield ((dirX, dirY), element)).toList
          val updatedNeighbors = results.map(_._2) ::: neighbors
          val updatedUnseen = unseenDirs diff results.map(_._1).toSet
          neighborsRec(updatedNeighbors, sight + 1, updatedUnseen)
        }
      }
      neighborsRec(List[Char](), 1, dirs)
    }

    def updateMatrix(
        vect: Vector[Vector[Char]],
        limitedSight: Boolean = true,
        maxNeighbors: Int = 4
    ): Vector[Vector[Char]] = {
      val maxSight =
        if (limitedSight) 1 else math.max(vect.length, vect(0).length)

      @tailrec
      def recUpdate(currState: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        val updates = for {
          i <- 0 until currState.length
          j <- 0 until currState(i).length
          currPos = currState(i)(j)
          updated = updatePos(
            currPos,
            neighbors(currState, i, j, maxSight),
            maxNeighbors
          )
          if (updated != currPos)
        } yield (updated, i, j)

        if (updates.isEmpty)
          currState
        else {
          val newState = updates.foldLeft(currState)((state, update) => {
            state.updated(
              update._2,
              state(update._2).updated(update._3, update._1)
            )
          })
          /* ^ Might get slow over big matrixes, possibly here a mutable in-place Java array could be better.. */
          // updates foreach {
          //   case (updated, i, j) => {
          //     currState(i)(j) = updated
          //   }
          // }
          recUpdate(newState)
        }
      }
      recUpdate(vect).toVector.map(_.toVector)
    }

    def countHashtags(vect: Vector[Vector[Char]]): Int =
      vect.flatten.filter(_ == '#').length

    println(
      "First answer: " + countHashtags(updateMatrix(vect))
    )
    println(
      "Second answer: " + countHashtags(updateMatrix(vect, false, 5))
    )
  }
}
