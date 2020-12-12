import scala.io.Source

case class Point(east: Int, north: Int) {
  def +(other: Point): Point =
    Point(east + other.east, north + other.north)

  def *(value: Int): Point =
    Point(east*value, north*value)

  def rotate(angle: Int): Point = {
    val c = math.cos(math.toRadians(angle)).toInt
    val s = math.sin(math.toRadians(angle)).toInt

    val newEast = east * c - north * s
    val newNorth = east * s + north * c

    Point(newEast, newNorth)
  }
}

case class Ship(pos: Point, dir: Point = Point(1, 0))

abstract class Action {
  def apply(point: Point): Point
}

abstract class SumAction extends Action {
  val point: Point
  def apply(other: Point): Point = point + other
}

abstract class RotateAction extends Action {
  val angle: Int
  def apply(point: Point): Point = point.rotate(angle)
}

case class North(value: Int) extends SumAction {
  val point = Point(0, value)
}
case class South(value: Int) extends SumAction {
  val point = Point(0, -value)
}
case class East(value: Int) extends SumAction {
  val point = Point(value, 0)
}
case class West(value: Int) extends SumAction {
  val point = Point(-value, 0)
}


case class Left(value: Int) extends RotateAction {
  val angle = value
}

case class Right(value: Int) extends RotateAction {
  val angle = -value
}


case class Forward(value: Int) extends Action {
  def apply(point: Point): Point =
    Point(point.east, point.north)*value
}


object Day12 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def parseLine(line: String): Action = {
      val word = line.slice(0, 1)
      val value = line.slice(1, line.length).toInt
      word match {
        case "R" => Right(value)
        case "L" => Left(value)
        case "W" => West(value)
        case "E" => East(value)
        case "S" => South(value)
        case "N" => North(value)
        case "F" => Forward(value)
      }
    }

    val list = Source.fromFile("input.txt").getLines.map(parseLine).toList

    val firstStarShip = list.foldLeft(Ship(Point(0,0)))((ship: Ship, action: Action) => action match {
      case action: SumAction => Ship(action(ship.pos), ship.dir)
      case action: RotateAction => Ship(ship.pos, action(ship.dir))
      case action: Forward => Ship(ship.pos + action(ship.dir), ship.dir)
    })

    val secondStarShip = list.foldLeft(Ship(Point(0,0), Point(10, 1)))((ship: Ship, action: Action) => action match {
      case action @ (_: SumAction | _: RotateAction) => Ship(ship.pos, action(ship.dir))
      case action: Forward => Ship(ship.pos + action(ship.dir), ship.dir)
    })

    println(
      "First answer: " + (math.abs(firstStarShip.pos.east) + math.abs(firstStarShip.pos.north))
    )
    println(
      "Second answer: " + (math.abs(secondStarShip.pos.east) + math.abs(secondStarShip.pos.north))
    )
  }
}




