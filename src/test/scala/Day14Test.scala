import org.scalatest.funspec.AnyFunSpec
import Day14._

import scala.io.Source

class Day14Test extends AnyFunSpec {
  describe("expandWall") {
    it("should return all coords of wall given two corners (head > tail)") {
      val res = expandWall((5, 1), List((1, 1)))
      val expected = List(
        (5, 1),
        (4, 1),
        (3, 1),
        (2, 1),
        (1, 1)
      )
      assert(res == expected)
    }
    it("should return all coords of wall given two corners (tail > head)") {
      val res = expandWall((1, 1), List((5, 1)))
      val expected = List(
        (5, 1),
        (4, 1),
        (3, 1),
        (2, 1),
        (1, 1)
      ).reverse
      assert(res == expected)
    }
  }
  describe("wall") {
    it("returns all coords of full run of a wall") {
      val res = wall("498,4 -> 498,6 -> 496,6")
      val expected = Set(
        (498, 4),
        (498, 5),
        (498, 6),
        (497, 6),
        (496, 6),
      )
      assert(res == expected)
    }
  }

  describe("dropSand") {
    it("adds sand that settles") {
      val cDims = CaveDimensions(0, 4, 0, 4)
      val takenLocations = Set(
        (0,4),
        (1,4),
        (2,4),
        (3,4),
        (4,4),
      )
      val res = dropSand(2, 0, takenLocations, cDims)
      val expected = ((2, 3) :: takenLocations.toList).toSet
      assert(res == expected)
    }
    it("sends sand to the abyss") {
      val cDims = CaveDimensions(0, 4, 0, 4)
      val takenLocations = Set(
        (0, 4),
        (1, 4),
        (2, 4),
      )
      val res = dropSand(2, 0, takenLocations, cDims)
      val expected = takenLocations
      assert(res == expected)
    }
    it("drops sand left") {
      val cDims = CaveDimensions(0, 4, 0, 4)
      val takenLocations = Set(
        (2, 3),
        (0, 4),
        (1, 4),
        (2, 4),
        (3, 4),
        (4, 4),
      )
      val res = dropSand(2, 0, takenLocations, cDims)
      val expected = ((1,3) :: takenLocations.toList).toSet
      assert(res == expected)
    }
    it("drops sand right") {
      val cDims = CaveDimensions(0, 4, 0, 4)
      val takenLocations = Set(
        (2, 3),
        (0, 4),
        (1, 4),
        (2, 4),
        (3, 4),
        (4, 4),
        (1, 3)
      )
      val res = dropSand(2, 0, takenLocations, cDims)
      val expected = ((3, 3) :: takenLocations.toList).toSet
      assert(res == expected)
    }
  }
  describe("part1") {
    it("should return 24 for test.txt") {
      val src = Source.fromFile("input/day14/test.txt")
      val lines = src.getLines().toList
      src.close()
      assert(part1(lines) == 24)
    }
  }
  describe("part2") {
    it("should return 93 for test.txt") {
      val src = Source.fromFile("input/day14/test.txt")
      val lines = src.getLines().toList
      src.close()
      assert(part2(lines) == 93)
    }
  }
}
