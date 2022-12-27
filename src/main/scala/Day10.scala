import java.beans.Statement
import scala.io.Source

object Day10 {


  trait Statement {
    def performOp: (Int, Int)
  }

  case class Addx(num: Int) extends Statement {
    override def performOp: (Int, Int) = (num, 2)
  }

  case class Noop() extends Statement {
    override def performOp: (Int, Int) = (0, 1)
  }

  def parseLine(line: String): Statement = {
    val addRE = "^addx (-?\\d+)".r
    val noopRE = "^noop".r
    line match {
      case addRE(num) => Addx(num.toInt)
      case noopRE() => Noop()
    }
  }

  case class ProgramCounter(stmt: Statement, cycle: Int, x: Long, signalStrengthSum: Long)

  def runProgram(program: Vector[ProgramCounter], cyclesOfInterest: Vector[Int]): ProgramCounter = {
    program.foldLeft(ProgramCounter(Noop(), 0, 1L, 0L)) { (acc, curr) => {
      val cycles = acc.cycle + curr.stmt.performOp._2
      val x = acc.x + curr.stmt.performOp._1
      if (cyclesOfInterest.contains(cycles)) {
        ProgramCounter(curr.stmt, cycles, x, acc.signalStrengthSum + cycles * x)
      }
      else if ((acc.cycle + 1 until cycles).count(cyclesOfInterest.contains(_)) != 0) {
        val coi = (acc.cycle + 1 until cycles).filter(cyclesOfInterest.contains(_)).head
        ProgramCounter(curr.stmt, cycles, x, acc.signalStrengthSum + acc.x * coi)
      } else
        ProgramCounter(curr.stmt, cycles, x, acc.signalStrengthSum)
    }
    }
  }

  def generateCyclesOfInterest(cyclesMax: Int): Vector[Int] =
    (20 to cyclesMax).toVector.filter(p => (p - 20) % 40 == 0)

  def part1(lines: Vector[String]): Long = {
    val pc = lines.map(parseLine).map(ProgramCounter(_, 0, 0L, 0L))
    val cyclesOfInterest = generateCyclesOfInterest(220)
    runProgram(pc, cyclesOfInterest).signalStrengthSum
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day10/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    println(s"Part1: ${part1(lines)}")
  }
}
