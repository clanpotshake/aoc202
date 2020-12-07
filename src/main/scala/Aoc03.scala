import scala.io.Source

object Aoc03 extends App {

  case class ForestRow(row: List[Char]) {
    val width: Int = row.length
    def isTree(i: Int): Boolean = row(i % width) == '#'
  }
  case class Forest(rows: List[ForestRow]) {
    val height: Int = rows.length
    def traverseWith(right: Int, down: Int): Long = {
      var x = 0
      var treeCount = 0L
      val yStep = 0 until height by down
      for (y <- yStep) {
        if (rows(y).isTree(x)) treeCount += 1
        x += right
      }
      treeCount
    }

  }

  val source = Source
    .fromResource("input_03.txt")
  val input = source
    .getLines()
    .map { line =>
      ForestRow(line.toCharArray.toList)
    }
  val forest = Forest(input.toList)
  source.close()
  val slopes =
    (1, 1) ::
      (3, 1) ::
      (5, 1) ::
      (7, 1) ::
      (1, 2) ::
      Nil
  val trees = slopes.map {
    case (x, y) =>
      forest.traverseWith(right = x, down = y)
  }
//  val trees = forest.traverseWith(right = 3, down = 1)
  println(s"${trees.product} found along path")
}
