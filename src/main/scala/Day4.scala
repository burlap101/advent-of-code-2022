import scala.io.Source

object Day4 {
  def part1(filename: String): Int = {
    val reader = Source.fromFile(filename)
    val cnt = reader.getLines().count(ln => {
      val pair = ln.split(',').map(seq => {
        val lims = seq.split('-')
        (lims(0).toInt to lims(1).toInt).toList
      })
      if (pair(0).length == pair(1).length) {
        pair(0).containsSlice(pair(1))
      } else {
        pair.maxBy(_.length).containsSlice(pair.minBy(_.length))
      }
    })
    reader.close()
    cnt
  }

  def part2(filename: String): Int = {
    val reader = Source.fromFile(filename)
    val cnt = reader.getLines().count(ln => {
      val pair = ln.split(',').map(seq => {
        val lims = seq.split('-')
        (lims(0).toInt to lims(1).toInt).toList
      })
      (pair(0) intersect pair(1)).nonEmpty
    })
    reader.close()
    cnt
  }

  def main(args: Array[String]): Unit = {
    println(s"Part1: ${part1("input/day4/actual.txt")}")
    println(s"Part2: ${part2("input/day4/actual.txt")}")
  }
}
