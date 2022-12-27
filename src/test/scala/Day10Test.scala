import org.scalatest.funspec.AnyFunSpec
import Day10._

import scala.io.Source
class Day10Test extends AnyFunSpec {
  describe("parseLine") {
    it("should return Addx(5)") {
      assert(parseLine("addx 5") == Addx(5))
    }
    it("should return Addx(-5)") {
      assert(parseLine("addx -5") == Addx(-5))
    }
    it("should return Noop()") {
      assert(parseLine("noop") == Noop())
    }
  }
  describe("runProgram") {
    it("should return correct x at end") {
      val prog = Vector(
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Addx(5), 0, 0, 0),
      )
      assert(runProgram(prog, Vector(0)).x == 11L)
    }
    it("should return correct x with negatives") {
      val program = Vector(
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Addx(-10), 0, 0, 0)
      )
      assert(runProgram(program, Vector(0)).x == -4L)
    }
    it("should return correct ssSum for one cycle of interest") {
      val program = Vector(
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Noop(), 0, 0, 0),
        ProgramCounter(Addx(5), 0, 0, 0)
      )
      assert(runProgram(program, Vector(4)).signalStrengthSum == 24L)
    }
    it("should return correct ssSum for two cycles of interest") {
      val program = Vector(
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Noop(), 0, 0, 0),
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Addx(5), 0, 0, 0),
      )
      assert(runProgram(program, Vector(3, 5)).signalStrengthSum == 73L)
    }
    it("should return correct ssSum for three cycles of interest") {
      val program = Vector(
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Addx(5), 0, 0, 0),
        ProgramCounter(Addx(-5), 0, 0, 0)
      )
      assert(runProgram(program, Vector(2, 3, 6)).signalStrengthSum == 66L)
    }
  }
  describe("cyclesOfInterest") {
    it("should return correct cyclesOfInterest") {
      assert(generateCyclesOfInterest(200) == Vector(20, 60, 100, 140, 180))
    }
  }
  describe("part1 step by step") {
    val src = Source.fromFile("input/day10/test.txt")
    val lines = src.getLines().toVector
    src.close()
    val pc = lines.map(parseLine).map(ProgramCounter(_, 0, 0L, 0L))
    it("should return 420 on the first coi") {
      assert(runProgram(pc, Vector(20)).signalStrengthSum == 420L)
    }
    it("should return 1140 for cycle 60") {
      assert(runProgram(pc, Vector(60)).signalStrengthSum == 1140L)
    }
    it("should return 1800 for cycle 100") {
      assert(runProgram(pc, Vector(100)).signalStrengthSum == 1800L)
    }
    it("should return 2940 for cycle 140") {
      assert(runProgram(pc, Vector(140)).signalStrengthSum == 2940L)
    }
    it("should return 2880 for cycle 180") {
      assert(runProgram(pc, Vector(180)).signalStrengthSum == 2880L)
    }
    it("should return 3960 for cycle 220") {
      assert(runProgram(pc, Vector(220)).signalStrengthSum == 3960L)
    }
  }
  describe("Day10.part1") {
    it("should return 13140 for test.txt") {
      val src = Source.fromFile("input/day10/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part1(lines) == 13140)
    }
  }
  describe("Day10.part2") {
    it("should produce the same output as the example") {
      val src = Source.fromFile("input/day10/test.txt")
      val lines = src.getLines().toVector
      src.close()
      val expected =
        """
          |##..##..##..##..##..##..##..##..##..##..
          |###...###...###...###...###...###...###.
          |####....####....####....####....####....
          |#####.....#####.....#####.....#####.....
          |######......######......######......####
          |#######.......#######.......#######.....
          |""".stripMargin.replaceAll("\n", "")
      val p2 = part2(lines)
      part2renderer(p2)
      assert(p2 == expected)
    }
  }
}
