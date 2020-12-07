import scala.io.Source
import scala.util.matching.Regex

object Aoc02 extends App {
  case class Rule(min: Int, max: Int, letter: Char)
  val partOneValidator: (Rule, String) => Boolean = {
    case (Rule(min, max, letter), password) =>
      var occurrences = 0
      password.foreach { c =>
        if (c == letter) occurrences += 1
      }
      min <= occurrences && occurrences <= max
  }
  val partTwoValidator: (Rule, String) => Boolean = {
    case (Rule(i, j, letter), password) =>
      password.charAt(i - 1) == letter ^ password.charAt(j - 1) == letter
  }
  val inputPattern: Regex = "([0-9]+)-([0-9]+) ([A-z]): (.+)".r
  def parse(in: String): Option[(Rule, String)] = {
    inputPattern.findFirstMatchIn(in) match {
      case Some(m) =>
        val min = m.group(1).toInt
        val max = m.group(2).toInt
        val char = m.group(3).charAt(0)
        val password = m.group(4)
        Some(Rule(min, max, char) -> password)
      case _ => None
    }
  }

  // min count - max count LETTER: password
  // 13-14 g: tggztfgdggngmglgg
  val source = Source
    .fromResource("input_02.txt")
  val input = source
    .getLines()
    .map(parse)
  val validLines = input.collect { case Some((rule, password)) => partTwoValidator(rule, password) }
//  val validLines = input.collect { case Some((rule, password)) => partOneValidator(rule, password) }
  val validCount = validLines.count(identity)
  source.close()

  println(s"$validCount passwords are valid")

}
