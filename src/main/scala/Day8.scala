import scala.io.Source
import annotation.tailrec


abstract class Instruction {
  def newIdx(idx: Int): Int = idx + 1
  def newAcc(acc: Int): Int = acc
}

case class Jump(value: Int) extends Instruction {
  override def newIdx(idx: Int) = idx + value
}
case class Nop(value: Int) extends Instruction
case class Acc(value: Int) extends Instruction {
  override def newAcc(acc: Int): Int = acc + value
}


object Day8 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def parseLine(line: String): Instruction = {
      val pattern = "([a-z]{3}) ([\\+\\-])(\\d+)".r
      line match {
        case  pattern(expr, sign, valueStr) => {
          val value = if (sign == "+") valueStr.toInt else -valueStr.toInt
          expr match {
            case "jmp" => Jump(value)
            case "nop" => Nop(value)
            case "acc" => Acc(value)
          }
        }
        case _ => throw new RuntimeException("Invalid Instruction")
      }
    }
    val vect = Source.fromFile(path).getLines.map(parseLine).toVector

    def traverseInstList(vect: Vector[Instruction]): (Boolean, Int) = {
      @tailrec
      def accTraverse(idx: Int, acc: Int, explored: Set[Int]): (Boolean, Int) = {
        val inst = vect(idx)
        val newIdx = inst.newIdx(idx)
        val newAcc = inst.newAcc(acc)
        if (newIdx >= vect.length)
          (true, newAcc)
        else if (explored.contains(newIdx))
          (false, newAcc)
        else
          accTraverse(newIdx, newAcc, explored + newIdx)
      }
      accTraverse(0, 0, Set[Int]())
    }

    def switchNopJump(vect: Vector[Instruction], idx: Int): Vector[Instruction] = {
      val newInst = vect(idx) match {
        case Jump(value) => Nop(value)
        case Nop(value) => Jump(value)
        case _ => throw new RuntimeException("Replacing wrong instruction")
      }
      vect.slice(0, idx) :+ newInst :++ vect.slice(idx + 1, vect.length)
    }

    def secondStar(vect: Vector[Instruction]): Int =
      (for {
        idx <- vect.view.zipWithIndex.filter(
          _._1 match {
            case (_: Jump | _: Nop ) => true
            case _: Acc => false
          }
        ).map(_._2)
        newVect = switchNopJump(vect, idx)
        (terminated, acc) = traverseInstList(newVect)
        if (terminated == true)
      } yield acc).head

    println(
      "First answer: " + traverseInstList(vect)._2
    )
    println(
      "Second answer: " + secondStar(vect)
    )
  }
}
