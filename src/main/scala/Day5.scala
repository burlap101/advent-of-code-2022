import java.util.Scanner
import java.util.regex.Pattern
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

object Day5 {

  class Ship() {
    var stacks: Map[Int, Stack] = Map()
    def createStacks(filename: String): Unit = {
      val reader = Source.fromFile(filename)
      reader.getLines().foreach(ln => {
        val cratePattern = "\\[([A-Z])\\]".r
        cratePattern.findAllMatchIn(ln).foreach(m => {
          if(!stacks.contains(m.start / 4 + 1)) {
            stacks = stacks + ((m.start / 4 + 1) -> new Stack())
          }
          stacks(m.start / 4 + 1).shimmy(m.group(1).charAt(0))
        })
      })
      reader.close()
    }

    def performMovement(command: String): Unit = {
      val re = "\\d+".r
      val movement: List[Int] = re.findAllMatchIn(command).toList.map(m => m.group(0).toInt)
      for (i <- 0 until movement.head) {
        val ch = stacks(movement(1)).pop()
        stacks(movement(2)).push(ch)
      }
    }

    def performMovementV2(command: String): Unit = {
      val re = "\\d+".r
      val movement: List[Int] = re.findAllMatchIn(command).toList.map(m => m.group(0).toInt)
      val chs = stacks(movement(1)).pop(movement.head)
      stacks(movement(2)).push(chs)
    }

    def peekTops(): String = {
      var result = ""
      val sortedKeys = stacks.keys.toSeq.sorted
      for (i <- sortedKeys) {
        result += stacks(i).peek()
      }
      result
    }
  }

  class Stack() {
    var containers: List[Char] = List()
    def push(ch: Char): Unit = { containers = ch :: containers }
    def push(chs: List[Char]): Unit = { containers = chs ::: containers}
    def pop(): Char = {
      val ch = containers.head
      containers = containers.tail
      ch
    }
    def pop(n: Int): List[Char] = {
      val chs = containers.slice(0, n)
      containers = containers.slice(n, containers.length)
      chs
    }
    def shimmy(ch: Char): Unit = {
      containers = containers.appended(ch)
    }
    def peek(): Char = containers.head
  }

  def part1(filename: String): String = {
    val ship = new Ship()
    ship.createStacks(filename)
    val reader = Source.fromFile(filename)
    val commandPattern = "^move .*".r
    reader.getLines().filter(ln => {
      commandPattern.matches(ln)
    }).foreach(cmd => ship.performMovement(cmd))
    ship.peekTops()
  }

  def part2(filename: String): String = {
    val ship = new Ship()
    ship.createStacks(filename)
    val reader = Source.fromFile(filename)
    val commandPattern = "^move .*".r
    reader.getLines().filter(ln => {
      commandPattern.matches(ln)
    }).foreach(cmd => ship.performMovementV2(cmd))
    ship.peekTops()
  }

  def main(args: Array[String]): Unit = {
    println(s"Part1: ${part1("input/day5/actual.txt")}")
    println(s"Part2: ${part2("input/day5/actual.txt")}")
  }
}
