object Aoc01 extends App {
  import scala.util.Try
  import scala.io.Source
  import java.io.FileNotFoundException

  val source = Source
    .fromResource("input_01.txt")
  val input = source
    .getLines()
    .map(_.toInt)
    .toList
  source.close()

  val Target = 2020

  var a: Int = 0
  var b: Int = 0
  var c: Int = 0

  for (i <- input.indices) {
    for (j <- 1 until input.length) {
      for (k <- 1 until input.length) {
        if (input(i) + input(j) + input(k) == Target) {
          a = input(i)
          b = input(j)
          c = input(k)
        }
      }
    }
  }
  println(a * b * c)

}
