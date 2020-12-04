import scala.io.Source
import scala.util.matching.Regex.MatchIterator
import scala.util.matching.Regex

case class Passport(fields: Map[String, String]) {
  val necessaryFields = Set(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  def hasNecessaryFields: Boolean = necessaryFields subsetOf fields.keys.toSet

  def checkPattern(pattern: Regex, str: String)(
      fun: MatchIterator => Boolean
  ): Boolean = {
    val it = pattern.findAllIn(str)
    if (it.isEmpty)
      false
    else
      fun(it)
  }

  def numberRange(
      pattern: Regex,
      str: String,
      greaterThan: Int,
      lessThan: Int
  ): Boolean =
    checkPattern(pattern, str)((it: MatchIterator) => {
      val num = it.matchData.next.group(1).toInt
      num >= greaterThan && num <= lessThan
    })

  def simpleMatch(pattern: Regex, str: String): Boolean =
    checkPattern(pattern, str)((_: MatchIterator) => true)

  def isValidField(field: (String, String)): Boolean = field match {
    case ("byr", value) => numberRange("^(\\d{4})$".r, value, 1920, 2002)
    case ("iyr", value) => numberRange("^(\\d{4})$".r, value, 2010, 2020)
    case ("eyr", value) => numberRange("^(\\d{4})$".r, value, 2020, 2030)
    case ("hgt", value) =>
      numberRange("^(\\d+)in$".r, value, 59, 76) ||
      numberRange("^(\\d+)cm$".r, value, 150, 193)
    case ("hcl", value) => simpleMatch("^\\#(?:\\d|[a-f]){6}$".r, value)
    case ("ecl", value) =>
      simpleMatch("^(?:amb|blu|brn|gry|grn|hzl|oth)$".r, value)
    case ("pid", value) => simpleMatch("^\\d{9}$".r, value)
    case ("cid", _)     => true
    case (_, _)         => false
  }

  def isValid = hasNecessaryFields && fields.forall(isValidField(_))
}

object Day4 {
  def main(args: Array[String]) {
    val path: String = {
      if (args.length == 0)
        "./input.txt"
      else
        args(0)
    }

    def getChunks(s: List[String]): List[List[String]] = {
      val (h, t) = s.span(_.nonEmpty)
      if (h.isEmpty) List.empty
      else h.flatMap(_.split(' ')) :: getChunks(t drop 1)
    }

    val list: List[List[String]] = getChunks(
      Source.fromFile(path).getLines.toList
    )

    def splitKeyValue(str: String): (String, String) = {
      val splitted = str.split(':')
      (splitted(0), splitted(1))
    }

    val passports = list.map(fields =>
      Passport(
        fields.map(splitKeyValue(_)).toMap
      )
    )

    println(
      "First answer: " + passports.filter(_.hasNecessaryFields).length
    )
    println(
      "Second answer: " + passports.filter(_.isValid).length
    )
  }
}
