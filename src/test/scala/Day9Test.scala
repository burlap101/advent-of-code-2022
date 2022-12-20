import Day9.{TailMovesResult, absMoveToCoords, calcTailMoves, coordsVisited, parseLine}
import org.scalatest.funspec.AnyFunSpec

import scala.io.Source

class Day9Test extends AnyFunSpec {
  describe("Day9.part1") {
    it("should return 13 for test input") {
      val src = Source.fromFile("input/day9/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(Day9.part1(lines) == 13)
    }
  }
  describe("Day9.calcTailMoves") {
    val src = Source.fromFile("input/day9/test.txt")
    val lines = src.getLines().toVector
    src.close()
    val lit = lines.map(parseLine)
    it("should return 3 for step1") {
      val tailPos = (0, 0)
      val l = lit(0)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 0), 3))
    }
    it(s"should return 3 for step 2") {
      val tailPos = (-1, 0)
      val l = lit(1)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((0, -1), 3))
    }
    it(s"should return 2 for step 3") {
      val tailPos = (0, -1)
      val l = lit(2)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((1, 0), 2))
    }
    it(s"should retunr 0 for step 4") {
      val tailPos = (1, 0)
      val l = lit(3)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((1, 1), 0))
    }
    it(s"should return 2 for step 5") {
      val tailPos = (1, 1)
      val l = lit(4)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 0), 2))
    }
    it(s"should return 0 for step 6") {
      val tailPos = (-1, 0)
      val l = lit(5)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 1), 0))
    }
    it(s"should return 3 for step 7") {
      val tailPos = (-1, 1)
      val l = lit(6)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((1, 0), 3))
    }
    it(s"should return 0 for step 8") {
      val tailPos = (1, 0)
      val l = lit(7)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 0), 0))
    }
    it(s"should return 0 for 1 step down when above") {
      val tailPos = (0, -1)
      val l = ('D', 1)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((0, 0), 0))
    }
    it(s"should return 0 for 1 step up when below") {
      val tailPos = (0, 1)
      val l = ('U', 1)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((0, 0), 0))
    }
    it(s"should return 0 for step up from below and to the right") {
      val tailPos = (-1, 1)
      val l = ('U', 2)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, -1), 0))
    }
    it(s"should return 0 for step left from the right and above") {
      val tailPos = (-1, -1)
      val l = ('L', 2)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((1, -1), 0))
    }
    it(s"should return 1 for step left from above to the left") {
      val tailPos = (1, -1)
      val l = ('L', 1)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((1, 0), 1))
    }
    it(s"should return 3 for step right from below to the right") {
      val tailPos = (-1, 1)
      val l = ('R', 3)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 0), 3))
    }
    it(s"should return 3 for step right from below to the left") {
      val tailPos = (1, 1)
      val l = ('R', 5)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((-1, 0), 3))
    }
    it(s"should return 0 for step up from on tail") {
      val tailPos = (0, 0)
      val l = ('U', 1)
      assert(calcTailMoves(tailPos, l) == TailMovesResult((0, -1), 0))
    }
  }
  describe("Day9.coordVisited") {
    it("should return a given set for tailpos below the head") {
      val tailPos = (0, -1)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Set((5, 0), (5, 1), (5, 2), (5, 3), (5,4)))
    }
    it("should return a given set for tailpos above the head") {
      val tailPos = (0, 1)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Set((5, 6), (5, 7), (5, 8), (5, 9), (5, 10)))
    }
    it("should return a given set for tailpos to the left of the head") {
      val tailPos = (-1, 0)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Set((0, 5), (1, 5), (2, 5), (3, 5), (4, 5)))
    }
    it("should return a given set for tailpos to the right of the head") {
      val tailPos = (1, 0)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Set((6, 5), (7, 5), (8, 5), (9, 5), (10, 5)))
    }
  }
  describe("Day9.absMoveToCoords") {
    it("should move left for 'L'") {
      val mv = ('L', 5)
      val originalPos = (5, 5)
      val result = absMoveToCoords(mv, originalPos)
      assert(result == (0, 5))
    }
    it("should move right for 'R'") {
      val mv = ('R', 5)
      val originalPos = (5, 5)
      val result = absMoveToCoords(mv, originalPos)
      assert(result == (10, 5))
    }
    it("should move up for 'U'") {
      val mv = ('U', 5)
      val originalPos = (5, 5)
      val result = absMoveToCoords(mv, originalPos)
      assert(result == (5, 10))
    }
    it("should move down for 'D'") {
      val mv = ('D', 5)
      val originalPos = (5, 5)
      val result = absMoveToCoords(mv, originalPos)
      assert(result == (5, 0))
    }
  }
}
