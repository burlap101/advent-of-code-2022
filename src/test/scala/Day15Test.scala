import org.scalatest.funspec.AnyFunSpec
import Day15._

import scala.io.Source

class Day15Test extends AnyFunSpec {
  describe("extractSensors") {
    it("should return a list of sensors") {
      val input: List[String] = List(
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
        "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
      )
      val expected = List(
        Sensor((2, 18), Beacon((-2, 15))),
        Sensor((9, 16), Beacon((10, 16))),
      )
      assert(extractSensors(input) == expected)
    }
  }
  describe("calcCoverageForRow") {
    it("should return a set of 3 spots consumed along row") {
      val sensors: List[Sensor] = List(
        Sensor((2, 0), Beacon((2, 4))),
      )
      val expected: Set[Long] = Set(1, 2, 3)
      assert(calcCoverageForRow(3, sensors) == expected)
    }
    it("should return a set of 5 spots consumed along row") {
      val sensors: List[Sensor] = List(
        Sensor((2, 0), Beacon((2, 4))),
        Sensor((4, 4), Beacon((6, 4))),
      )
      val expected: Set[Long] = Set(1, 2, 3, 4, 5)
      assert(calcCoverageForRow(3, sensors) == expected)
    }
  }
  describe("part1") {
    it("should return 26 for test.txt for y=10") {
      val src = Source.fromFile("input/day15/test.txt")
      val lines = src.getLines().toList
      src.close()
      assert(part1(lines, 10) == 26L)
    }
  }
  describe("part2") {
    it("should return 56000011 for test.txt") {
      val src = Source.fromFile("input/day15/test.txt")
      val lines = src.getLines().toList
      src.close()
      assert(part2(lines, 20L) == "56000011")
    }
  }
  describe("Sensor.nextPointAlongOutside") {
    val sensor = Sensor((2,2), Beacon((2,4)))
    it("should return next point from top position clockwise") {
      val result = sensor.nextPointAlongOutside((2,5))
      assert(result.contains((3, 4)))
    }
    it("should return next point along from between top and right") {
      val result = sensor.nextPointAlongOutside((3,4))
      assert(result.contains((4,3)))
    }
    it("should return none for point beyond outside") {
      val result = sensor.nextPointAlongOutside((5,5))
      assert(result.isEmpty)
    }
    it("should return none for point inside cloud") {
      val result = sensor.nextPointAlongOutside((2,3))
      assert(result.isEmpty)
    }
  }
  describe("Sensor.firstPoint") {
    it("should return top dead centre for within xyMax") {
      val s = Sensor((5, 5), Beacon((4,4)), 50)
      assert(s.firstPoint().contains((5, 8)))
    }
    it("should return next point around when going y out-of-bounds") {
      val s = Sensor((2,2), Beacon((3,4)), 5)
      assert(s.firstPoint().contains((3,5)))
    }
    it("should return top dead centre wh") {
      val s = Sensor(())
    }
  }

}
