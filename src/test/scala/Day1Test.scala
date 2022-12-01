import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {
  test("Day1.part1") {
    assert(Day1.part1("input/day1/test.txt") == 24000L)
  }
  test("Day1.part2") {
    assert(Day1.part2("input/day1/test.txt") == 45000L)
  }
  test("tolong works") {
    assert(" 10000".trim.toLong == 10000L)
  }
}

