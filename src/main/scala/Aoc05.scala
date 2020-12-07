import scala.io.Source

object Aoc05 extends App {
  def seatId(str: String): Int = {
    val (rowInput, colInput) = str.splitAt(7)
    val scrubbedRow = rowInput.map {
      case 'F' => '0'
      case 'B' => '1'
    }
    val scrubbedCol = colInput.map {
      case 'R' => '1'
      case 'L' => '0'
    }

    val row = Integer.parseInt(scrubbedRow, 2)
    val col = Integer.parseInt(scrubbedCol, 2)
    row * 8 + col
  }

  // find highest seat ID
  val source = Source
    .fromResource("input_05.txt")
  val seatIds = source
    .getLines()
    .map(seatId)
    .toList
    .sorted

  println(s"highest seat ID is: ${seatIds.max}")

  val mySeat = seatIds
    .sliding(2)
    .find {
      case a :: b :: Nil =>
        b - a > 1
    }
    .map { case a :: b :: Nil => (a + b) / 2 }
  println(s"my seat ID is: ${mySeat.get}")

//  val test1 = "BFFFBBFRRR" // row 70, column 7, seat ID 567.
//  val test2 = "FFFBBBFRRR" // row 14, column 7, seat ID 119.
//  val test3 = "BBFFBBFRLL" // row 102, column 4, seat ID 820.
//  println(seatId(test1))
//  println(seatId(test2))
//  println(seatId(test3))
  source.close()
}
