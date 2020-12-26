import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object LeftArithmetic extends RegexParsers {
  def plus: Parser[BigInt => BigInt] = "+" ~ term ^^ { case "+" ~ t2 => _ + t2 }
  def mult: Parser[BigInt => BigInt] = "*" ~ term ^^ { case "*" ~ t2 => _ * t2 }
  def term: Parser[BigInt] = factor ~ rep(plus | mult) ^^ { case t1 ~ t2 =>
    t2.foldLeft(t1)((acc, f) => f(acc))
  }
  def factor: Parser[BigInt] = num | "(" ~> term <~ ")"
  def num: Parser[BigInt] = "\\d+".r ^^ (BigInt(_))

  def parse(input: String): BigInt = {
    parseAll(term, input).get
  }
}

object InvertedArithmetic extends RegexParsers {
  def expr: Parser[BigInt] = term ~ rep(mult) ^^ { case t1 ~ t2 =>
    t2.foldLeft(t1)((acc, f) => f(acc))
  }
  def plus: Parser[BigInt => BigInt] = "+" ~ term ^^ { case "+" ~ t2 => _ + t2 }
  def term: Parser[BigInt] = factor ~ rep(plus) ^^ { case t1 ~ t2 =>
    t2.foldLeft(t1)((acc, f) => f(acc))
  }
  def mult: Parser[BigInt => BigInt] = "*" ~ term ^^ { case "*" ~ t2 => _ * t2 }
  def factor: Parser[BigInt] = num | "(" ~> expr <~ ")"
  def num: Parser[BigInt] = "\\d+".r ^^ (BigInt(_))

  def parse(input: String): BigInt = {
    parseAll(expr, input).get
  }
}

object Day18 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    val list = Source.fromFile(path).getLines.toList

    def reverseExpr(s: String): String =
      s.foldLeft("")((acc, ch) => {
        val newch =
          if (ch == ')')
            '('
          else if (ch == '(')
            ')'
          else
            ch
        acc + newch
      }).reverse

    println(
      "First answer: " + list
        .map(expr => LeftArithmetic.parse(reverseExpr(expr)))
        .sum
    )
    println(
      "Second answer: " + list.map(InvertedArithmetic.parse(_)).sum
    )
  }
}
