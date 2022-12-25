import Day9.{Section, TailMovesResult, absMoveToCoords, calcTailMoves, coordsVisited, parseLine, part2, performMoves}
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
      assert(result == Vector((5, 0), (5, 1), (5, 2), (5, 3), (5,4)))
    }
    it("should return a given set for tailpos above the head") {
      val tailPos = (0, 1)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Vector((5, 6), (5, 7), (5, 8), (5, 9), (5, 10)))
    }
    it("should return a given set for tailpos to the left of the head") {
      val tailPos = (-1, 0)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Vector((0, 5), (1, 5), (2, 5), (3, 5), (4, 5)))
    }
    it("should return a given set for tailpos to the right of the head") {
      val tailPos = (1, 0)
      val mvCnt = 5
      val headAbsPos = (5, 5)
      val result = coordsVisited(mvCnt, headAbsPos, tailPos)
      assert(result == Vector((6, 5), (7, 5), (8, 5), (9, 5), (10, 5)))
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
  describe("Day9.Section.moveViaHead") {
    it("should return expected coords for head movement vector") {
      val headMoves = Vector(
        (0,1),
        (0,2),
        (0,3),
        (0,4),
        (0,5)
      )
      val expectedTailMoves = Vector(
        (0,0),
        (0,1),
        (0,2),
        (0,3),
        (0,4),
      )
      val section = new Section()
      assert(section.moveViaHead(headMoves)==expectedTailMoves)
    }
    it("should move as expected for diagonal change of position") {
      val section = new Section((3,0), (-1,0))
      val headMoves = Vector(
        (4,1)
      )
      val expectedResult = Vector(
        (3,1)
      )
      assert(section.moveViaHead(headMoves)== expectedResult)
    }
  }
  describe("Day9.Section.calcTailMovesV2") {
    it("should return expected tail moves vector for move") {
      val section = new Section(headPos = (5, 5), relTailPos = (0, -1))
      val expected = Vector(
        (5,5),
        (5,6),
        (5,7),
        (5,8),
        (5,9),
      )
      val result = section.calcTailMovesV2(('U', 5))
      assert(result == expected)
    }
  }
  describe("Day9.performMoves") {
    it("should move the coordinates expected for a 2 section rope after one move") {
      val sections = Vector(new Section((4, 0), (-1, 0)), new Section((3, 0), (-1, 0)))
      val headMoves = sections.head.moveViaCommand(('U', 4))
      val expected = Vector(
        (2, 0),
        (3, 1),
        (3, 1),
        (4, 2),
      )
      //assert(sections.tail.head.moveViaHead(headMoves) == expected)
      assert(performMoves(sections.tail, headMoves) == expected)
    }
    it("should move the coordinates expected for a 3 section rope after one move") {
      val sections = Vector(new Section((4,0), (-1,0)), new Section((3,0), (-1, 0)), new Section((2,0), (-1,0)))
      val headMoves = sections.head.moveViaCommand(('U', 4))
      val expected = Vector(
        (1, 0),
        (2, 1),
        (2, 1),
        (3, 2),
      )
      assert(performMoves(sections.tail, headMoves) == expected)
    }
    it("should return the coordinates expected for a 3 section rope after two moves") {
      val sections = Vector(new Section((4, 0), (-1, 0)), new Section((3, 0), (-1, 0)), new Section((2, 0), (-1, 0)))
      val moves = Vector(
        ('U', 4),
        ('L', 3),
      )
      val result = moves.flatMap(mv => {
        val headMoves = sections.head.moveViaCommand(mv)
        println(headMoves)
        performMoves(sections.tail, headMoves)
      })
      val expected = Vector(
        (1, 0),
        (2, 1),
        (2, 1),
        (3, 2),
        (3, 2),
        (3, 2),
        (3, 2),
      )
      assert(result == expected)
    }
    it("should return expected result for 3 sections from right then up") {
      val sections = Vector.fill(2){new Section}
      val moves = Vector(
        ('R', 1),
        ('U', 2)
      )
      val expectedPositions = Vector(
        (1,2),
        (1,1),
        (0,0)
      )
      val result = moves.flatMap(mv =>{
        val headMoves = sections.head.moveViaCommand(mv)
        performMoves(sections.tail, headMoves)
      })
      val rpositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(rpositions == expectedPositions)
      assert(result == Vector.fill(3){(0,0)})
      ;
    }
    it("should perform moves as expected for a 3 section rope up by 1") {
      val sections = Vector(new Section((4,1), (-1, -1)), new Section((3,0), (-1,0)))
      val headMoves = sections.head.moveViaHead(Vector((4,2)))
      val result = performMoves(sections.tail, headMoves)
      val expectedPositions = Vector(
        (4,2),
        (4,1),
        (3,1)
      )
      val resPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(result == Vector((3,1)))
      assert(resPositions==expectedPositions)
    }
  }
  describe("Day9.Section.moveViaCommand") {
    it("should return expected coordinates for a move left") {
      val section = new Section((5, 5), (1, 0))
      val expected = Vector(
        (5,5),
        (4,5),
        (3,5),
      )
      assert(section.moveViaCommand(('L', 3))==expected)
    }
    it("should return expected coordinates for a move up") {
      val section = new Section((4, 1), (-1, -1))
      val expected = Vector(
        (4,1)
      )
      val result = section.moveViaCommand(('U', 1))
      assert(section.headPos == (4,2))
      assert(section.relTailPos == (0,-1))
      assert(result== expected)
    }
  }
  describe("Day9.performMoves for 10 section") {
    it("should not move the tail for R5 from origin") {
      val sections = Vector.fill(9){new Section}
      val mv = ('R', 5)
      val headMoves = sections.head.moveViaCommand(mv)
      val result = performMoves(sections.tail, headMoves)
      sections.foreach(s => println(s.headPos, s.relTailPos, s.tailAbsPos()))
      assert(result==Vector.fill(5){(0,0)})
    }
    it("should not move the tail for U8 from origin") {
      val sections = (1 to 5).reverse.toVector.map(p => new Section((p, 0),(-1, 0))) ++ Vector.fill(4){new Section}
      val mv = ('U', 8)
      val headMoves = sections.head.moveViaCommand(mv)
      val result = performMoves(sections.tail, headMoves)
      val expected = Vector(
        (5,8),
        (5,7),
        (5,6),
        (5,5),
        (5,4),
        (4,4),
        (3,3),
        (2,2),
        (1,1),
        (0,0),
      )
      val allSectionPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      sections.foreach(s => println(s.headPos, s.relTailPos, s.tailAbsPos()))
      assert(allSectionPositions==expected)
      assert(result==Vector.fill(8){(0,0)})
    }
  }
  describe("Vector.fill") {
    it("should produce 10 sections") {
      val sections:Vector[Section] = Vector.fill(10){new Section()}
      assert(sections.length==10)
      assert(sections(5).isInstanceOf[Section])
    }
  }
  describe("Day9.part2") {
    it("should return 1 for original test.txt") {
      val src = Source.fromFile("input/day9/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part2(lines) == 1)
    }
    it("should return 36 for the larger test2.txt") {
      val src = Source.fromFile("input/day9/test2.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part2(lines) == 36)
    }
  }
  describe("performMoves mapping all for test.txt") {
    it("should move sections to expected from origin to R4") {
      val sections = Vector.fill(9){new Section}
      val headMoves = sections.head.moveViaCommand(('R', 4))
      val result = performMoves(sections.tail, headMoves)
      val expectedPositions = Vector(
        (4, 0),
        (3, 0),
        (2, 0),
        (1, 0)
      ) ++ Vector.fill(6){(0,0)}
      val resultPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      sections.foreach(s => println(s.headPos, s.relTailPos))
      assert(resultPositions == expectedPositions)
      assert(result == Vector.fill(4){(0,0)})
    }
    it("should move sections to expected from R4 -> U4") {
      val sections = Vector(
        new Section((4,0),(-1,0)),
        new Section((3,0),(-1,0)),
        new Section((2,0),(-1,0)),
        new Section((1,0),(-1,0)),
        new Section((0,0),(0,0)),
        new Section((0,0),(0,0)),
        new Section((0,0),(0,0)),
        new Section((0,0),(0,0)),
        new Section((0,0),(0,0)),
      )
      val expectedPositions = Vector(
        (4,4),
        (4,3),
        (4,2),
        (3,2),
        (2,2),
        (1,1),
      ) ++ Vector.fill(4){(0,0)}
      val headMoves = sections.head.moveViaCommand(('U', 4))
      val result = performMoves(sections.tail, headMoves)
      val resultPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      sections.foreach(s => println(s.headPos, s.relTailPos))
      assert(resultPositions == expectedPositions)
      assert(result == Vector.fill(4) {
        (0, 0)
      })
    }
  }
  describe("mapping each position after every head move of R4 -> U4") {
    it("should only move head first") {
      val sections = Vector(
        new Section((4, 0), (-1, 0)),
        new Section((3, 0), (-1, 0)),
        new Section((2, 0), (-1, 0)),
        new Section((1, 0), (-1, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
      )
      val expectedPositions = Vector(
        (4, 1),
        (3, 0),
        (2, 0),
        (1, 0),
      ) ++ Vector.fill(6) {
        (0, 0)
      }
      val headMoves = sections.head.moveViaCommand(('U', 1))
      performMoves(sections.tail, headMoves)

      val resPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(expectedPositions == resPositions)
    }
    it("should move head-4 as expected for second step") {
      val sections = Vector(
        new Section((4, 0), (-1, 0)),
        new Section((3, 0), (-1, 0)),
        new Section((2, 0), (-1, 0)),
        new Section((1, 0), (-1, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
      )
      val expectedPositions = Vector(
        (4,2),
        (4,1),
        (3,1),
        (2,1),
        (1,1),
      ) ++ Vector.fill(5){(0,0)}
      val headMoves = sections.head.moveViaCommand(('U',2))
      performMoves(sections.tail, headMoves)
      val resPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(resPositions == expectedPositions)
    }
    it("should move head-1 as expected for third step") {
      val sections = Vector(
        new Section((4, 0), (-1, 0)),
        new Section((3, 0), (-1, 0)),
        new Section((2, 0), (-1, 0)),
        new Section((1, 0), (-1, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
      )
      val expectedPositions = Vector(
        (4, 3),
        (4, 2),
        (3, 1),
        (2, 1),
        (1, 1),
      ) ++ Vector.fill(5) {
        (0, 0)
      }
      val headMoves = sections.head.moveViaCommand(('U', 3))
      performMoves(sections.tail, headMoves)
      val resPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(resPositions == expectedPositions)
    }
    it("should move head-5 as expected for fourth step") {
      val sections = Vector(
        new Section((4, 0), (-1, 0)),
        new Section((3, 0), (-1, 0)),
        new Section((2, 0), (-1, 0)),
        new Section((1, 0), (-1, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
        new Section((0, 0), (0, 0)),
      )
      val expectedPositions = Vector(
        (4, 4),
        (4, 3),
        (4, 2),
        (3, 2),
        (2, 2),
        (1, 1)
      ) ++ Vector.fill(4) {
        (0, 0)
      }
      val headMoves = sections.head.moveViaCommand(('U', 4))
      performMoves(sections.tail, headMoves)
      val resPositions = Vector(sections.head.headPos) ++ sections.map(_.tailAbsPos())
      assert(resPositions == expectedPositions)
    }
  }
}
