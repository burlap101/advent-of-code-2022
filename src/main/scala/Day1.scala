import scala.{+:, ::}
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.SortedSet
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day1 {
  def ingestFile(fileName: String): List[String] = {
    val reader = Source.fromFile(fileName)
    val result = reader.getLines().toList
    reader.close()
    result
  }
  def part1(fileName: String): Long = {
    var accum = 0L
    var max = 0L
    ingestFile(fileName).foreach(line => {
      Try(line.trim.toLong) match {
        case Success(value) => accum = accum + value
        case Failure(_) => {
          if (accum > max) max = accum
          accum = 0L
        }
      }
    })
    max
  }

  def part2(fileName: String): Long = {
    var cals: List[Long] = List[Long]()
    var accum = 0L
    ingestFile(fileName).foreach(line => {
      Try(line.trim.toLong) match {
        case Success(value) => accum += value
        case Failure(_) => {
          cals = accum :: cals
          accum = 0L
        }
      }
    })
    if (accum != 0) cals = accum :: cals
    cals.sortWith((a,b) => a > b).slice(0, 3).sum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part1: ${part1("input/day1/actual.txt")}")
    println(s"Part2: ${part2("input/day1/actual.txt")}")
  }
}
