import scala.io.Source
import annotation.tailrec


abstract class Instruction {
  def apply(state: MachineState): MachineState =
    MachineState(state.idx +  1, state.acc)
}

case class Jump(value: Int) extends Instruction {
  override def apply(state: MachineState): MachineState =
    MachineState(state.idx + value, state.acc)
}
case class Nop(value: Int) extends Instruction
case class Acc(value: Int) extends Instruction {
  override def apply(state: MachineState): MachineState =
    MachineState(state.idx + 1, state.acc + value)
}

case class MachineState(idx: Int, acc: Int, isTerminated: Boolean = false) {
  def terminated: MachineState =
    MachineState(idx, acc, true)
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

    def traverseInstList(vect: Vector[Instruction]): MachineState = {
      @tailrec
      def accTraverse(state: MachineState, explored: Set[Int]): MachineState = {
        val inst = vect(state.idx)
        val newState = inst(state)
        if (newState.idx >= vect.length)
          newState.terminated
        else if (explored.contains(newState.idx))
          newState
        else
          accTraverse(newState, explored + newState.idx)
      }
      accTraverse(MachineState(0, 0), Set[Int]())
    }

    def switchNopJump(vect: Vector[Instruction], idx: Int): Vector[Instruction] = {
      val newInst = vect(idx) match {
        case Jump(value) => Nop(value)
        case Nop(value) => Jump(value)
        case _ => throw new RuntimeException("Replacing wrong instruction")
      }
      vect.updated(idx, newInst)
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
        state = traverseInstList(newVect)
        if (state.isTerminated == true)
      } yield state.acc).head

    println(
      "First answer: " + traverseInstList(vect).acc
    )
    println(
      "Second answer: " + secondStar(vect)
    )
  }
}
