import scala.io.Source
import scala.annotation.tailrec

case class Cube(x: Int, y: Int, z: Int, w: Int = 0, active: Boolean = false) {
  def areClose(a: Int, b: Int): Boolean = {
    val diff = a - b
    diff <= 1 && diff >= -1
  }

  def neighbors(others: List[Cube]) = {
    others.filter { c =>
      areClose(c.x, x) && areClose(c.y, y) &&
      areClose(c.z, z) && areClose(c.w, w) &&
      (c.x != x || c.y != y || c.z != z || c.w != w)
    }
  }

  def evolve(others: List[Cube]): Cube = {
    val numActive = neighbors(others).filter(_.active).length
    val newState = if (active) {
      numActive == 2 || numActive == 3
    } else {
      numActive == 3
    }
    Cube(x, y, z, w, newState)
  }

  def canEqual(a: Any) = a.isInstanceOf[Cube]

  override def equals(other: Any): Boolean = other match {
    case other: Cube =>
      other.canEqual(this) &&
      x == other.x && y == other.y &&
      z == other.z && w == other.w
    case _ => false
  }
}

object Day17 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val list = Source.fromFile(path).getLines.toList
    val cubes = for {
      (row, rowId) <- list.map(_.zipWithIndex).zipWithIndex
      (el, colId) <- row
      active = if (el == '#') true else false
    } yield Cube(rowId, colId, 0, 0, active)

    def doCycles(
        cubeList: List[Cube],
        secondStar: Boolean = false,
        cycle: Int = 0,
        maxCycle: Int = 6
    ): List[Cube] = {
      if (cycle == maxCycle)
        cubeList
      else {
        val xs = cubeList.map(_.x).distinct
        val ys = cubeList.map(_.y).distinct
        val zs = cubeList.map(_.z).distinct
        val ws = cubeList.map(_.w).distinct
        val wsRange = if (secondStar) (ws.min - 1 to ws.max + 1) else (0 to 0)
        val newCubeList = (for {
          x <- xs.min - 1 to xs.max + 1
          y <- ys.min - 1 to ys.max + 1
          z <- zs.min - 1 to zs.max + 1
          w <- wsRange
          newCube = Cube(x, y, z, w, false)
          if (!cubeList.contains(newCube))
        } yield (newCube)).toList ::: cubeList
        val evolvedCubes = newCubeList.map(_.evolve(cubeList)).filter(_.active)

        doCycles(evolvedCubes, secondStar, cycle + 1, maxCycle)
      }
    }

    println(
      "First answer: " + doCycles(cubes).length
    )
    println(
      "Second answer: " + doCycles(cubes, true).length
    )
  }
}
