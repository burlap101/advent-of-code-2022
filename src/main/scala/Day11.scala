import scala.io.Source
import scala.util.matching.Regex

object Day11 {

  class Monkey(
                var items: Vector[Long],
                val operator: Char,
                val operand: Int,
                val divisor: Int,
                val trueMonkey: Int,
                val falseMonkey: Int,
              ) {
    var inspections = 0L

    def operation(): Unit = {
      items = items.map(item => operator match {
        case '+' => if (operand == 0) item + item else item + operand
        case '-' => if (operand == 0) item - item else item - operand
        case '*' => if (operand == 0) item * item else item * operand
        case '/' => if (operand == 0) item / item else item / operand
      }).map(_ / 3)
      inspections += items.length
    }

    def operationPt2(commonDiv: Long): Unit = {
      items = items.map(item => operator match {
        case '+' => if (operand == 0) item + item else item + operand
        case '-' => if (operand == 0) item - item else item - operand
        case '*' => if (operand == 0) item * item else item * operand
        case '/' => if (operand == 0) item / item else item / operand
      }).map(_ % commonDiv)
      inspections += items.length
    }

    /**
     * performs the test
     *
     * @return (trueItems: Vector[Int], falseItems: Vector[Int])
     */
    def test(): (Vector[Long], Vector[Long]) =
      (items.filter(_ % divisor == 0), items.filter(_ % divisor != 0))
  }

  def getMonkeyLines(lines: Vector[String]): Vector[Vector[String]] = {
    val (monkeyLines, remainingLines) = lines.splitAt(lines.indexWhere(p => "^\\s*$".r.matches(p)))
    if (remainingLines.length == 6 && monkeyLines.length == 0) {
      Vector(remainingLines)
    } else {
      Vector(monkeyLines) ++ getMonkeyLines(remainingLines.drop(1))
    }
  }

  def getMonkeys(lines: Vector[String]): Vector[Monkey] = {
    getMonkeyLines(lines).map(parseMonkey)
  }

  val siRE: Regex = "^\\s*Starting items: ([\\d, ]+)$".r
  val opRE: Regex = "^\\s*Operation: new = old ([\\+\\-\\*\\/]) (old|\\d+)".r
  val divRE: Regex = "^\\s*Test: \\D+(\\d+)".r

  def parseMonkey(lines: Vector[String]): Monkey = {
    val startingItems = lines(1) match {
      case siRE(itemsStr) => itemsStr.split(",").map(_.trim).map(_.toLong).toVector
    }
    val oppers: (Char, Int) = lines(2) match {
      case opRE(operator, operand) => (operator.trim.head, if (operand == "old") 0 else operand.trim.toInt)
    }
    val divisor = lines(3) match {
      case divRE(div) => div.trim.toInt
    }
    val trueMonkey = lines(4).split(" ").last.trim.toInt
    val falseMonkey = lines(5).split(" ").last.trim.toInt
    new Monkey(startingItems, oppers._1, oppers._2, divisor, trueMonkey, falseMonkey)
  }

  def performRound(monkeys: Vector[Monkey]): Vector[Monkey] = {
    monkeys.map(m => {
      m.operation()
      val testRes = m.test()
      monkeys(m.trueMonkey).items = monkeys(m.trueMonkey).items ++ testRes._1
      monkeys(m.falseMonkey).items = monkeys(m.falseMonkey).items ++ testRes._2
      m.items = Vector()
      m
    })
  }

  def performRoundPt2(monkeys: Vector[Monkey], commonDiv: Long): Vector[Monkey] = {
    monkeys.map(m => {
      m.operationPt2(commonDiv)
      val testRes = m.test()
      monkeys(m.trueMonkey).items = monkeys(m.trueMonkey).items ++ testRes._1
      monkeys(m.falseMonkey).items = monkeys(m.falseMonkey).items ++ testRes._2
      m.items = Vector()
      m
    })
  }

  def part1(lines: Vector[String]): Long = {
    val monkeys = getMonkeys(lines)
    (1 to 20).foreach(p => {
      performRound(monkeys)
    })
    val insps = monkeys.map(_.inspections).sorted.reverse
    insps.head * insps(1)
  }

  def part2(lines: Vector[String]): Long = {
    val monkeys = getMonkeys(lines)
    val commonDiv = monkeys.map(_.divisor).product
    (1 to 10000).foreach(p => {
      performRoundPt2(monkeys, commonDiv)
    })
    val insps = monkeys.map(_.inspections).sorted.reverse
    insps.head * insps(1)
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day11/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    println(s"Part1: ${part1(lines)}")
    println(s"Part2: ${part2(lines)}")
  }
}
