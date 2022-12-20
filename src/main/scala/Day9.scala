import scala.io.Source

object Day9 {

  def parseLine(input: String): (Char, Int) = {
    val sp = input.split(" ")
    (sp(0).charAt(0), sp(1).toInt)
  }

  def foldBack(moves: Int): Int = {
    if (moves >= 2) moves - 2 else 0
  }

  def turn90(moves: Int): Int = {
    if (moves >= 1) moves - 1 else 0
  }

  def turn90OnOne(moves: Int): Int = {
    if (moves >= 2) moves - 2 else 0
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

  def coordsVisited(mvCnt: Int, headAbsPos: (Int, Int), tailPos: (Int, Int)): Set[(Int, Int)] = {
    if (mvCnt == 0) Set() else {
      tailPos match {
        case (0, -1) =>
          (headAbsPos._2 - mvCnt until headAbsPos._2).map((headAbsPos._1, _)).toSet
        case (0, 1) =>
          (headAbsPos._2 + 1 to headAbsPos._2 + mvCnt).map((headAbsPos._1, _)).toSet
        case (-1, 0) =>
          (headAbsPos._1 - mvCnt until headAbsPos._1).map((_, headAbsPos._2)).toSet
        case (1, 0) =>
          (headAbsPos._1 + 1 to headAbsPos._1 + mvCnt).map((_, headAbsPos._2)).toSet
      }
    }
  }

  case class TailMovesResult(tailPos: (Int, Int), mvCnt: Int)

  case class TailMovesResultV2(tailPos: (Int, Int), mvCnt: Int, headAbsCoords: (Int, Int))

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

  var coordsVisited: Set[(Int, Int)] = Set((0, 0))

  def calcTailMovesV2(tailPos: (Int, Int), move: (Char, Int), headAbsPos: (Int, Int)): TailMovesResultV2 = {
    val tm = calcTailMoves(tailPos, move)
    val headPos = absMoveToCoords(move, headAbsPos)
    val cv = coordsVisited(tm.mvCnt, headPos, tm.tailPos)
    val cnt = cv.count(!coordsVisited.contains(_))
    coordsVisited = cv ++ coordsVisited
    TailMovesResultV2(tm.tailPos, cnt, headPos)
  }

  def part1(lines: Vector[String]): Long = {
    var relTailPos = (0, 0)
    var headPos = (0, 0)
    lines.map(parseLine)
      .map(mv => {
        val res = calcTailMovesV2(relTailPos, mv, headPos)
        relTailPos = res.tailPos
        headPos = res.headAbsCoords
        res.mvCnt
      })
      .sum + 1
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day9/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    println(s"Part1: ${part1(lines)}")
  }
}
