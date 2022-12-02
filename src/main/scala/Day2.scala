import scala.io.Source

object Day2 {
  class Game(first: Char, second: Char) {
    val choices1 = "ABC"
    val choices2 = "XYZ"
    private def outcome(): Int = {
      choices1.indexOf(first) match {
        case 0 => choices2.indexOf(second) match {
          case 0 => 3
          case 1 => 6
          case 2 => 0
        }
        case 1 => choices2.indexOf(second) match {
          case 0 => 0
          case 1 => 3
          case 2 => 6
        }
        case 2 => choices2.indexOf(second) match {
          case 0 => 6
          case 1 => 0
          case 2 => 3
        }
      }
    }
    def yourScore(): Int = choices2.indexOf(second) + 1 + outcome()
  }

  def part1(filename: String): Long = {
    val reader = Source.fromFile(filename)
    var accum = 0L
    reader.getLines().foreach(ln => {
      val turns = ln.split(" ").map[Char](_.charAt(0))
      accum += new Game(turns(0), turns(1)).yourScore()
    })
    reader.close()
    accum
  }

  class GameV2(first: Char, outcome: Char) {
    val choices1 = "ABC"
    val outcomes = "XYZ"

    private def choiceScore(): Int = {
      choices1.indexOf(first) match {
        case 0 => outcomes.indexOf(outcome) match {
          case 0 => 3
          case 1 => 1
          case 2 => 2
        }
        case 1 => outcomes.indexOf(outcome) match {
          case 0 => 1
          case 1 => 2
          case 2 => 3
        }
        case 2 => outcomes.indexOf(outcome) match {
          case 0 => 2
          case 1 => 3
          case 2 => 1
        }
      }
    }
    def score(): Int = choiceScore() + outcomes.indexOf(outcome) * 3
  }

  def part2(filename: String): Long = {
    val reader = Source.fromFile(filename)
    var accum = 0L
    reader.getLines().foreach(ln => {
      val g = ln.split(" ").map[Char](_.charAt(0))
      accum += new GameV2(g(0), g(1)).score()
    })
    reader.close()
    accum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part1: ${part1("input/day2/actual.txt")}")
    println(s"Part2: ${part2("input/day2/actual.txt")}")
  }
}
