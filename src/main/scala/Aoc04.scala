import scala.io.Source
import scala.util.Try

object Aoc04 extends App {
  trait Year {
    def year: Int
    def min: Int
    def max: Int
    final def isValid: Boolean = min <= year && year <= max
  }
  case class BirthYear(year: Int) extends Year {
    val min = 1920
    val max = 2002
  }
  case class IssueYear(year: Int) extends Year {
    val min = 2010
    val max = 2020
  }
  case class ExpirationYear(year: Int) extends Year {
    val min = 2020
    val max = 2030
  }
  sealed trait Height {
    def value: Int
    val min: Int
    val max: Int
    final def isValid: Boolean = min <= value && value <= max
  }
  object Height {
    private val heightRegex = "([0-9]+)((in|cm))".r
    def apply(str: String): Option[Height] =
      heightRegex
        .findFirstMatchIn(str)
        .map { m =>
          val v = m.group(1).toInt
          val unit = m.group(2)
          unit match {
            case "in" => Inches(v)
            case "cm" => Centimeters(v)
          }
        }
  }
  case class Inches(value: Int) extends Height {
    val min = 59
    val max = 76
  }
  case class Centimeters(value: Int) extends Height {
    val min = 150
    val max = 193
  }
  case class Passport(
    birthYear: Option[BirthYear] = None,
    issueYear: Option[IssueYear] = None,
    expirationYear: Option[ExpirationYear] = None,
    height: Option[Height] = None,
    hairColor: Option[String] = None,
    eyeColor: Option[String] = None,
    passportID: Option[String] = None,
    countryID: Option[String] = None
  ) {
    private def hairIsValid(value: String): Boolean =
      "#[0-9a-f]{6}".r.findFirstMatchIn(value).isDefined
    private def eyeIsValid(value: String): Boolean =
      value == "amb" || value == "blu" || value == "brn" || value == "gry" ||
        value == "grn" || value == "hzl" || value == "oth"
    private def pidIsValid(value: String): Boolean = value.length == 9 && value.forall(_.isDigit)
    def isValid: Boolean =
      birthYear.exists(_.isValid) &&
        issueYear.exists(_.isValid) &&
        expirationYear.exists(_.isValid) &&
        height.exists(_.isValid) &&
        hairColor.exists(hairIsValid) &&
        eyeColor.exists(eyeIsValid) &&
        passportID.exists(pidIsValid)
    true
  }
  def groupIterator(it: Iterator[String]): Iterator[List[String]] = new Iterator[List[String]] {
    override def hasNext: Boolean = it.hasNext
    override def next: List[String] = it.takeWhile(_ != "").toList
  }

  val keyRegex = "([a-z]{3}):(.+)".r

  val source = Source
    .fromResource("input_04.txt")

  val groups = groupIterator(source.getLines())
  val passports = groups.map { group =>
    val input = group.flatMap(_.split("\\s").toList)
    var current = Passport()
    for (v <- input) {
      val m = keyRegex.findFirstMatchIn(v)
      m.foreach { n =>
        val key = n.group(1)
        val kv = n.group(2)
        key match {
          case "byr" => current = current.copy(birthYear = Try(kv.toInt).toOption.map(BirthYear))
          case "iyr" => current = current.copy(issueYear = Try(kv.toInt).toOption.map(IssueYear))
          case "eyr" =>
            current = current.copy(expirationYear = Try(kv.toInt).toOption.map(ExpirationYear))
          case "hgt" => current = current.copy(height = Height(kv))
          case "hcl" => current = current.copy(hairColor = Some(kv))
          case "ecl" => current = current.copy(eyeColor = Some(kv))
          case "pid" => current = current.copy(passportID = Some(kv))
          case "cid" => current = current.copy(countryID = Some(kv))
        }
      }
    }
    current
  }.toList

  source.close()
  println(s"File contains ${passports.count(_.isValid)} valid passports")

}
