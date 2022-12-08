import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {
  test("Day6.part1") {
    val scenarios: List[(String, Int)] = List(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 7,
      "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 5,
      "nppdvjthqldpwncqszvftbrmjlhg" -> 6,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 10,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 11,
    )
    scenarios.foreach(scenario => {
      val it: Iterator[Char] = scenario._1.iterator
      val result: Int = Day6.part1(it)
      assert(result == scenario._2)
    })
  }
  test("Day6.part2") {
    val scenarios: List[(String, Int)] = List(
      "mjqjpqmgbljsphdztnvjfqwrcgsmlb" -> 19,
      "bvwbjplbgvbhsrlpgdmjqwftvncz" -> 23,
      "nppdvjthqldpwncqszvftbrmjlhg" -> 23,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 29,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" -> 26,
    )
    scenarios.foreach(scenario => {
      val it: Iterator[Char] = scenario._1.iterator
      val result: Int = Day6.part2(it)
      assert(result == scenario._2)
    })
  }
}
