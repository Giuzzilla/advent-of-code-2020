import scala.io.Source

object Day22 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    type Player = List[Int]

    def parsePlayer(s: String): List[Int] = {
      val lines = s.split("\n").toList
      lines.slice(1, lines.length).map(_.toInt)
    }

    def play(player1: Player, player2: Player, firstStar: Boolean, history: Set[Player] = Set[Player]()): (Player, Int) = {
      if (player1.isEmpty)
        (player2, 2)
      else if(player2.isEmpty)
        (player1, 1)
      else {
        val (newPlayer1, newPlayer2) = if (!firstStar && history.contains(player1)) {
          (player1, List())
        } else if (!firstStar && player1.tail.length >= player1.head && player2.tail.length >= player2.head) {
          val (_, winnerId) = play(
            player1.tail.slice(0, player1.head),
            player2.tail.slice(0, player2.head),
            false,
            Set[Player]()
          )
          if (winnerId == 1) 
            (player1.tail ::: (player1.head :: player2.head :: Nil), player2.tail)
          else
            (player1.tail, player2.tail ::: (player2.head :: player1.head :: Nil))
        } else if (player1.head >= player2.head)
          (player1.tail ::: (player1.head :: player2.head :: Nil), player2.tail)
        else
          (player1.tail, player2.tail ::: (player2.head :: player1.head :: Nil))
        val newHistory = history | Set(player1)
        play(newPlayer1, newPlayer2, firstStar, newHistory)
      }
    }


    val list = Source.fromFile(path).getLines.mkString("\n").split("\n\n").toList.map(parsePlayer(_))

    val star1Winner = play(list(0), list(1), true)._1
    val star2Winner = play(list(0), list(1), false)._1

    println(
      "First answer: " + star1Winner.reverse.zipWithIndex.map{ case (value, idx) => value * (idx + 1)}.sum
    )
    println(
      "Second answer: " + star2Winner.reverse.zipWithIndex.map{ case (value, idx) => value * (idx + 1)}.sum
    )
  }
}
