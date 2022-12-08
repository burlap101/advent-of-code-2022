import scala.io.Source

object Day6 {

  def part1(it: Iterator[Char]): Int = it.sliding(4).takeWhile(_.toSet.size < 4).length + 4

  def part2(it: Iterator[Char]): Int = it.sliding(14).takeWhile(_.toSet.size < 14).length + 14

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day6/actual.txt")
    val (itp1, itp2) = src.iterator.duplicate
    println(s"Part1: ${part1(itp1)}")
    println(s"Part2: ${part2(itp2)}")
    src.close()
  }
}
