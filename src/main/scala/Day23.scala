import scala.collection.mutable.{ArrayDeque, ListBuffer}
import scala.annotation.tailrec

object Day23 {
  def main(args: Array[String]) {
    val input: String = {
      if (args.length == 0)
        throw new IllegalArgumentException("Must provide an input for Day 15")
      else
        args(0)
    }

    val listInp = input.split("").map(_.toInt).toList

    def nextEl(
        deque: ArrayDeque[Int],
        afterMap: Map[Int, ListBuffer[Int]]
    ): (Int, Map[Int, ListBuffer[Int]]) = {
      val el = deque.removeHead(true)
      val newMap = if (afterMap.contains(el)) {
        afterMap(el) foreach {
          n => deque.prepend(n)
        }
        afterMap - el
      } else
        afterMap
      (el, newMap)
    }

    @tailrec
    def nextEls(
        deque: ArrayDeque[Int],
        afterMap: Map[Int, ListBuffer[Int]],
        n: Int,
        valueAcc: ListBuffer[Int] = ListBuffer[Int](),
        iter: Int = 0
    ): (ListBuffer[Int], Map[Int, ListBuffer[Int]]) = {
      if (iter == n) {
        (valueAcc, afterMap)
      } else {
        val (newValue, newMap) = nextEl(deque, afterMap)
        nextEls(deque, newMap, n, valueAcc :+ newValue, iter + 1)
      }
    }

    def solver(
        inp: List[Int],
        upTo: Int,
        maxIters: Int
    ): (ArrayDeque[Int], Map[Int, ListBuffer[Int]]) = {
      val deque = ArrayDeque(
        (inp :++ List.tabulate(upTo - inp.max)(_ + 1).map(_ + inp.max)): _*
      )
      val initMap =
        Map.empty[Int, ListBuffer[Int]].withDefaultValue(ListBuffer[Int]())

      @tailrec
      def recSolve(
          iter: Int,
          afterMap: Map[Int, ListBuffer[Int]]
      ): Map[Int, ListBuffer[Int]] = {
        if (iter >= maxIters)
          afterMap
        else {
          val (extractedCups, newMap) = nextEls(deque, afterMap, 4)
          val currCup = extractedCups.head
          val nextThree = extractedCups.tail
          val prevs =
            (1 until currCup).reverse.view.filter(i => !nextThree.contains(i))
          val destCup =
            if (prevs.isEmpty)
              (upTo - 2 to upTo).reverse.filter(!nextThree.contains(_)).head
            else prevs.head
          deque.append(currCup)
          recSolve(
            iter + 1,
            newMap + (destCup -> newMap(destCup).prependedAll(
              nextThree.reverse
            ))
          )
        }
      }
      val map = recSolve(0, initMap)
      (deque, map)
    }

    val (dequeFirst, mapFirst) = solver(listInp, listInp.max, 100)
    val (valsFirst, _) = nextEls(dequeFirst, mapFirst, listInp.length)


    val (dequeSecond, mapSecond) = solver(listInp, 1000000, 10000000)
    
    @tailrec
    def findOne(deque: ArrayDeque[Int], map: Map[Int, ListBuffer[Int]], currEl: Int = -1): BigInt = {
      if (currEl == 1) {
        val (els, _) = nextEls(deque, map, 2)
        els.map(BigInt(_)).reduce(_ * _)
      } else {
        val (newEl, newMap) = nextEl(deque, map)
        findOne(deque, newMap, newEl)
      }
    }

    println(
      "First answer: " + valsFirst.mkString("")
    )
    println(
      "Second answer: " + findOne(dequeSecond, mapSecond)
    )
  }
}