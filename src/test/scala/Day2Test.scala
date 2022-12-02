import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {
  test("Day2.part1") {
    assert(Day2.part1("input/day2/test.txt") == 15)
  }
  test("Day2.part2") {
    assert(Day2.part2("input/day2/test.txt") == 12)
  }
}
