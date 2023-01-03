import org.scalatest.funspec.AnyFunSpec

import scala.io.Source
import Day12._
class Day12Test extends AnyFunSpec {
  describe("part1") {
    it("should return 31 for test.txt") {
      val src = Source.fromFile("input/day12/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part1(lines) == 31)
    }
  }
  describe("getEndNode") {
    it("should return correct endnode for test.txt") {
      val src = Source.fromFile("input/day12/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(getEndNode(lines) == Node(None, 'z', (2, 5), 0))
    }
  }
  describe("getStartNode") {
    it("should return correct startnode for test.txt") {
      val src = Source.fromFile("input/day12/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(getStartNode(lines) == Node(None, 'a', (0, 0), 0))
    }
  }
  describe("Frontier.insert") {
    val frontier = new Frontier(Node(None, 'z', (2,5), 0))
    val startNode = Node(None, 'S', (0, 0), 0)
    it("should add nodes") {
      assert(frontier.isEmpty)
      frontier.insert(Node(Some(startNode), 'f', (0, 1), 1))
      assert(!frontier.isEmpty)
    }
  }
  describe("part1 step by step") {
    val src = Source.fromFile("input/day12/test.txt")
    val trueLines = src.getLines().toVector
    src.close()
    val endNode = getEndNode(trueLines)
    val startNode = getStartNode(trueLines)
    val frontier = new Frontier(endNode)
    val lines = trueLines.map(_.map(el => if (el == 'E') endNode.elevation else if (el == 'S') startNode.elevation else el))
    startNode.availableActions(lines).foreach(frontier.insert)
    it("should load frontier with expected nodes") {
      assert(frontier.nodes.length == 2)
      assert(frontier.nodes == Vector(Node(Some(startNode), 'a', (0,1), 1), Node(Some(startNode), 'a', (1, 0), 1)))
    }
  }
  describe("allAs") {
    val src = Source.fromFile("input/day12/test.txt")
    val lines = src.getLines().toVector
    src.close()
    val endNode = getEndNode(lines)
    it("should return all elevation a from the test.txt input") {
      assert(allAs(lines, endNode).length == 5)
    }
    it("should return sorted by f()") {
      val startNodes = allAs(lines, endNode)
      assert(startNodes.map(_.f(endNode)).sliding(2).count(p => p(0) > p(1)) == 0)
    }
  }
  describe("part2") {
    it("should return 29 for test.txt") {
      val src = Source.fromFile("input/day12/test.txt")
      val lines = src.getLines().toVector
      src.close()
      assert(part2(lines) == 29)
    }
  }
}
