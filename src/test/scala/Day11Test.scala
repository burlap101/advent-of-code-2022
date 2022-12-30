import org.scalatest.funspec.AnyFunSpec
import Day11._

import scala.io.Source

class Day11Test extends AnyFunSpec {
  describe("regex matching") {
    it("should work for starting items") {
      val input = "  Starting items: 79, 98"
      assert(siRE.matches(input))
      val result: Vector[Int] = input match {
        case siRE(values) => values.split(",").map(_.trim.toInt).toVector
      }

      assert(result == Vector(79, 98))
    }
    it("should work for operation params") {
      val input = "  Operation: new = old * 19"
      assert(opRE.matches(input))
    }
    it("should split out monkey lines") {
      val src = Source.fromFile("input/day11/test.txt")
      val lines = src.getLines().toVector
      src.close()
      val (monkeyLines, remainingLines) = lines.splitAt(lines.indexWhere(p => "^\\s*$".r.matches(p)))
      assert(monkeyLines.length == 6)
      assert(remainingLines.drop(1).length == 20)
    }
  }
  describe("parseMonkey") {
    it("returns an expected Monkey") {
      val lines =
        """|Monkey 0:
           |  Starting items: 79, 98
           |  Operation: new = old * 19
           |  Test: divisible by 23
           |    If true: throw to monkey 2
           |    If false: throw to monkey 3
           |          |""".stripMargin
      val result: Monkey = parseMonkey(lines.split("\n").toVector)
      val expected: Monkey = new Monkey(
        Vector(79, 98),
        '*',
        19,
        23,
        2,
        3
      )
      assert(result.items == expected.items)
      assert(result.operator == expected.operator)
      assert(result.operand == expected.operand)
      assert(result.trueMonkey == expected.trueMonkey)
      assert(result.falseMonkey == expected.falseMonkey)
    }
  }
  describe("getMonkeys") {
    it("should return 4 monkeys for the test.txt input") {
      val src = Source.fromFile("input/day11/test.txt")
      val lines = src.getLines().toVector
      src.close()
      val monkeys = getMonkeys(lines)
      assert(monkeys.length == 4)
    }
  }
  describe("getMonkeyLines") {
    it("should return length four for the test.txt input") {
      val src = Source.fromFile("input/day11/test.txt")
      val lines = src.getLines().toVector
      src.close()
      val monkeyLines = getMonkeyLines(lines)
      assert(monkeyLines.length == 4)
    }
  }
  describe("performRound") {
    val src = Source.fromFile("input/day11/test.txt")
    val lines = src.getLines().toVector
    src.close()
    it("should have same state as example for one round from test.txt") {
      val monkeys = getMonkeys(lines)
      val items = performRound(monkeys).map(_.items)
      val expected: Vector[Vector[Int]] = Vector(
        Vector(20, 23, 27, 26),
        Vector(2080, 25, 167, 207, 401, 1046),
        Vector(),
        Vector(),
      )
      assert(items == expected)
    }
  }
  describe("performRoundPt2") {
    val src = Source.fromFile("input/day11/test.txt")
    val lines = src.getLines().toVector
    src.close()
    it("should have same state of inspections as example for 1 round from test.txt") {
      val monkeys = getMonkeys(lines)
      val commonDiv = monkeys.map(_.divisor).product
      val insps = performRoundPt2(monkeys, commonDiv).map(_.inspections)
      val expected: Vector[Long] = Vector(
        2,
        4,
        3,
        6
      )
      assert(insps == expected)
    }
    it("should have same state of inspections as example for 20 rounds from test.txt") {
      val monkeys = getMonkeys(lines)
      val commonDiv = monkeys.map(_.divisor).product
      val insps = (1 to 20).map(i => performRoundPt2(monkeys, commonDiv)).last.map(_.inspections)
      val expected: Vector[Long] = Vector(
        99,
        97,
        8,
        103
      )
      assert(insps == expected)
    }
    it("should have same state of inspections as example for 1000 rounds from test.txt") {
      val monkeys = getMonkeys(lines)
      val commonDiv = monkeys.map(_.divisor).product
      val insps = (1 to 1000).map(i => performRoundPt2(monkeys, commonDiv)).last.map(_.inspections)
      val expected: Vector[Long] = Vector(
        5204,
        4792,
        199,
        5192,
      )
      assert(insps == expected)
    }
  }
  describe("part1") {
    it("should return 10605 for test.txt") {
      val src = Source.fromFile("input/day11/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part1(lines) == 10605L)
    }
  }
  describe("part2") {
    it("should return 2713310158 for test.txt") {
      val src = Source.fromFile("input/day11/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part2(lines) == 2713310158L)
    }
  }
}
