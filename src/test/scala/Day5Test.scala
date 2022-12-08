import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {
  test("Ship.createStacks") {
    val ship = new Day5.Ship()
    ship.createStacks("input/day5/test.txt")
    assert(ship.stacks.size == 3)
    assert(ship.stacks(1).containers == List('N', 'Z'))
    assert(ship.stacks(2).containers == List('D', 'C', 'M'))
    assert(ship.stacks(3).containers == List('P'))
  }
  test("Ship.performMovement") {
    val ship = new Day5.Ship()
    ship.createStacks("input/day5/test.txt")
    ship.performMovement("move 1 from 2 to 1")
    assert(ship.stacks(1).containers == List('D', 'N', 'Z'))
    assert(ship.stacks(2).containers == List('C', 'M'))
    ship.performMovement("")
    assert(ship.stacks(1).containers == List())
    assert(ship.stacks(3).containers == List('Z', 'N', 'D', 'P'))
  }
  test("command pattern match") {
    val re = "^move .*".r
    assert(re.matches("move 3 from 1 to 3"))
  }
  test("Stack.pushpop") {
    val s = new Day5.Stack()
    s.push('A')
    s.push('B')
    assert(s.pop() == 'B')
    assert(s.pop() == 'A')
  }
  test("Day5.part1") {
    assert(Day5.part1("input/day5/test.txt") == "CMZ")
  }
  test("Regex group") {
    val re = "\\[([A-Z])\\]".r
    val result = re.findFirstMatchIn("[A] ") match {
      case Some(m) => m.group(1)
      case None => ""
    }
    assert(result == "A")
  }
  test("Day5.part2") {
    assert(Day5.part2("input/day5/test.txt") == "MCD")
  }
}
