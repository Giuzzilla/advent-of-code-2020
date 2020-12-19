import scala.io.Source
import scala.annotation.tailrec

sealed trait Rule {
  val id: Int
}
case class CharRule(id: Int, char: String) extends Rule
case class RuleList(id: Int, others: List[List[Int]]) extends Rule

object Day19 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def parseRule(s: String): Rule = {
      val split1 = s.split(": ")
      val idx = split1(0).toInt
      val lst: List[String] = split1(1).split(" \\| ").toList

      val patternWord = "\"(\\w)\"".r
      val rule: Rule = lst.head match {
        case patternWord(word) => CharRule(idx, word)
        case _ =>
          RuleList(
            idx,
            lst.foldLeft(List[List[Int]]())((acc, curr) =>
              acc appended curr.split(" ").map(_.toInt).toList
            )
          )
      }
      rule
    }

    def buildRegex(
        rules: Map[Int, Rule],
        rule: Rule,
        depth: Int,
        maxDepth: Int
    ): String = {
      if (depth > maxDepth)
        ""
      else {
        rule match {
          case CharRule(_, char) => char
          case RuleList(_, others) =>
            "(?:" + others
              .map(
                _.map((i: Int) =>
                  buildRegex(rules, rules(i), depth + 1, maxDepth)
                ).mkString
              ).mkString("|") +
            ")"
        }
      }
    }

    val list = Source.fromFile(path).getLines.toList
    val modified =
      list.updated(122, "8: 42 | 42 8").updated(93, "11: 42 31 | 42 11 31")

    val (rules1 :: rules2 :: Nil) =
      Seq(list, modified).map((lst: List[String]) => lst.slice(0, 139).map {
        (s: String) => {
          val rule = parseRule(s)
          rule.id -> rule
        }
      }.toMap)

    val messages = list.slice(140, list.length)

    val (reg1 :: reg2 :: Nil) = Seq(rules1, rules2).map((ruleMap: Map[Int, Rule]) => buildRegex(ruleMap, ruleMap(0), 0, messages.map(_.length).max).r)


    println(
      "First answer: " + messages
        .map((msg: String) => (reg1 findFirstIn msg) match {
          case Some(matched) => matched == msg
          case None => false
        })
        .filter(_ == true)
        .length
    )
    println(
      "Second answer: " + messages
        .map((msg: String) => (reg2 findFirstIn msg) match {
          case Some(matched) => matched == msg
          case None => false
        })
        .filter(_ == true)
        .length
    )
  }
}
