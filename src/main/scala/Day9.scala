import scala.annotation.tailrec
import scala.io.Source

object Day9 {

  def parseLine(input: String): (Char, Int) = {
    val sp = input.split(" ")
    (sp(0).charAt(0), sp(1).toInt)
  }

  def moveToCoords(mv: (Char, Int)): (Int, Int) = {
    mv._1 match {
      case 'R' => (mv._2, 0)
      case 'L' => (-mv._2, 0)
      case 'U' => (0, mv._2)
      case 'D' => (0, -mv._2)
    }
  }

  def absMoveToCoords(mv: (Char, Int), originalPos: (Int, Int)): (Int, Int) = {
    mv._1 match {
      case 'R' => (mv._2 + originalPos._1, originalPos._2)
      case 'L' => (originalPos._1 - mv._2, originalPos._2)
      case 'U' => (originalPos._1, originalPos._2 + mv._2)
      case 'D' => (originalPos._1, originalPos._2 - mv._2)
    }
  }

  def absMoveToCoordsV2(mv: (Char, Int), originalPos: (Int, Int)): Vector[(Int, Int)] = {
    mv._1 match {
      case 'R' =>
        (originalPos._1 + 1 to mv._2 + originalPos._1).toVector.map((_, originalPos._2))
      case 'L' =>
        (originalPos._1 - mv._2 until originalPos._1).reverse.toVector.map((_, originalPos._2))
      case 'U' =>
        (originalPos._2 + 1 to originalPos._2 + mv._2).toVector.map((originalPos._1, _))
      case 'D' =>
        (originalPos._2 - mv._2 until originalPos._2).reverse.toVector.map((originalPos._1, _))
    }
  }

  def coordsVisited(mvCnt: Int, headAbsPos: (Int, Int), tailPos: (Int, Int)): Vector[(Int, Int)] = {
    if (mvCnt == 0) Vector() else {
      tailPos match {
        case (0, -1) =>
          (headAbsPos._2 - mvCnt until headAbsPos._2).toVector.map((headAbsPos._1, _))
        case (0, 1) =>
          (headAbsPos._2 + 1 to headAbsPos._2 + mvCnt).toVector.map((headAbsPos._1, _))
        case (-1, 0) =>
          (headAbsPos._1 - mvCnt until headAbsPos._1).toVector.map((_, headAbsPos._2))
        case (1, 0) =>
          (headAbsPos._1 + 1 to headAbsPos._1 + mvCnt).toVector.map((_, headAbsPos._2))
      }
    }
  }

  case class TailMovesResult(tailPos: (Int, Int), mvCnt: Int)

  def calcTailMoves(tailPos: (Int, Int), move: (Char, Int)): TailMovesResult = {
    val mvCoords = moveToCoords(move)
    val diff = (tailPos._1 - mvCoords._1, tailPos._2 - mvCoords._2)
    var mvCnt = 0
    val tailZero: Vector[Int] = Vector(
      if (math.abs(diff._2) > 1) {
        0
      } else diff._1,
      if (math.abs(diff._1) > 1) {
        0
      } else diff._2
    )
    mvCnt += {
      val mx = tailZero.map(v => math.abs(v)).max
      if (mx == 0) 0 else mx - 1
    }
    val newTailPos: (Int, Int) = (
      if (tailZero(0) > 0) 1 else if (tailZero(0) < 0) -1 else 0,
      if (tailZero(1) > 0) 1 else if (tailZero(1) < 0) -1 else 0
    )
    TailMovesResult(newTailPos, mvCnt)
  }

  class Section(var headPos: (Int, Int) = (0, 0), var relTailPos: (Int, Int) = (0, 0)) {
    var coordsSeen: Set[(Int, Int)] = Set(tailAbsPos())

    def calcTailMovesV2(move: (Char, Int)): Vector[(Int, Int)] = {
      val tm = calcTailMoves(relTailPos, move)
      val nextHeadPos = absMoveToCoords(move, headPos)
      val cv = coordsVisited(tm.mvCnt, nextHeadPos, tm.tailPos)
      coordsSeen = cv.toSet ++ coordsSeen
      relTailPos = tm.tailPos
      headPos = nextHeadPos
      cv
    }

    def tailAbsPos(): (Int, Int) = {
      (headPos._1 + relTailPos._1, headPos._2 + relTailPos._2)
    }

    def moveViaCommand(move: (Char, Int)): Vector[(Int, Int)] = {
      val headMoves = absMoveToCoordsV2(move, headPos)
      moveViaHead(headMoves)
    }

    def moveViaHead(headCoordsSeen: Vector[(Int, Int)]): Vector[(Int, Int)] = {
      val cv = headCoordsSeen.map(coords => {
        val currTailPos = tailAbsPos()
        headPos = coords
        val diff = (headPos._1 - currTailPos._1, headPos._2 - currTailPos._2)
        if (math.abs(diff._1) == 2 && math.abs(diff._2) == 2) {
          relTailPos = (if (diff._1 > 0) -1 else 1, if (diff._2 > 0) -1 else 1)
        } else if (math.abs(diff._1) > 1) {
          relTailPos = (if (diff._1 > 0) -1 else 1, 0)
        } else if (math.abs(diff._2) > 1) {
          relTailPos = (0, if (diff._2 > 0) -1 else 1)
        } else {
          relTailPos = (-diff._1, -diff._2)
        }
        tailAbsPos()
      })
      coordsSeen = cv.toSet ++ coordsSeen
      cv
    }
  }

  def part1(lines: Vector[String]): Long = {
    val section = new Section()
    lines.map(parseLine)
      .foreach(mv => {
        section.calcTailMovesV2(mv)
      })
    section.coordsSeen.size
  }

  @tailrec
  def performMoves(sections: Vector[Section], headMoves: Vector[(Int, Int)]): Vector[(Int, Int)] = {
    if (sections.size == 1) {
      sections.head.moveViaHead(headMoves)
    } else {
      performMoves(sections.tail, sections.head.moveViaHead(headMoves))
    }
  }
  def part2(lines: Vector[String]): Int = {
    val sections: Vector[Section] = Vector.fill(9){new Section()}
    lines.map(parseLine)
      .flatMap(mv => {
        val headMoves = sections.head.moveViaCommand(mv)
        performMoves(sections.tail, headMoves)
      })
      .toSet
      .size
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day9/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    println(s"Part1: ${part1(lines)}")
    println(s"Part2: ${part2(lines)}")
  }
}
