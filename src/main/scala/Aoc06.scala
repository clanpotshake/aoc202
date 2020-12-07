import scala.io.Source

object Aoc06 extends App {

  def groupIterator(it: Iterator[String]): Iterator[List[String]] = new Iterator[List[String]] {
    override def hasNext: Boolean = it.hasNext
    override def next: List[String] = it.takeWhile(_ != "").toList
  }
  val source = Source
    .fromResource("input_06.txt")
  val groups = groupIterator(source.getLines()).toList
  var groupYesSet = collection.mutable.Set[Char]()
  val yesesPartOne = groups.map { answers =>
    // answers for the whole group
    answers.foreach { answer =>
      // a person in the group's answers
      answer.foreach { yes =>
        groupYesSet.add(yes)
      }
    }
    val numYes = groupYesSet.size
    groupYesSet.clear()
    numYes
  }
  val yesesPartTwo = ???

  println(s"Total num yes: ${yeses.sum}")
  source.close()
}
