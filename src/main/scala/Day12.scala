import scala.io.Source

//noinspection ScalaWeakerAccess
object Day12 {

  case class Node(parent: Option[Node], elevation: Char, coords: (Int, Int), g: Int) {
    def f(endNode: Node): Double = {
      g + h(endNode);
    }

    def h(endNode: Node): Double = {
      math.sqrt(
        math.pow(endNode.coords._1 - coords._1, 2) + math.pow(endNode.coords._2 - coords._2, 2)
      )
    }

    def availableActions(lines: Vector[String]): Vector[Node] = {
      Vector(
        (coords._1 + 1, coords._2),
        (coords._1 - 1, coords._2),
        (coords._1, coords._2 + 1),
        (coords._1, coords._2 - 1),
      ).filter(p => p._1 >= 0 && p._1 < lines.length)
        .filter(p => p._2 >= 0 && p._2 < lines(0).length)
        .filter(p => {
          lines(p._1)(p._2) <= elevation + 1
        })
        .map(p => childNode(p, lines(p._1)(p._2)))
    }

    def childNode(childCoords: (Int, Int), childLetter: Char): Node = {
      Node(Some(this), childLetter, childCoords, g + 1)
    }

    def availableActionsPt2(lines: Vector[String]): Vector[Node] = {
      Vector(
        (coords._1 + 1, coords._2),
        (coords._1 - 1, coords._2),
        (coords._1, coords._2 + 1),
        (coords._1, coords._2 - 1),
      ).filter(p => p._1 >= 0 && p._1 < lines.length)
        .filter(p => p._2 >= 0 && p._2 < lines.head.length)
        .filter(p => {
          lines(p._1)(p._2) >= elevation - 1
        })
        .map(p => childNode(p, lines(p._1)(p._2)))
    }
  }

  class Frontier(val endNode: Node) {
    var nodes: Vector[Node] = Vector()

    def isEmpty: Boolean = nodes.isEmpty

    def pop(): Node = {
      val n = nodes.head
      nodes = nodes.tail
      n
    }

    def insert(node: Node): Unit = {
      val nds = nodes.filter(nd => nd.coords == node.coords)
      if (nds.isEmpty) {
        nodes = node +: nodes
        nodes = nodes.sortBy(_.f(endNode))
      } else if (nds.head.g > node.g) {
        val splitted = nodes.splitAt(nodes.indexOf(nds.head))
        nodes = (splitted._1 ++ splitted._2.tail.prepended(node)).sortBy(_.f(endNode))
      }
    }
  }

  def getStartNode(lines: Vector[String]): Node = {
    val m = lines.indices.find(lines(_).contains('S'))
    val n = lines(m.get).indexOf('S')
    Node(None, 'a', (m.get, n), 0)
  }

  def getEndNode(lines: Vector[String]): Node = {
    val m = lines.indices.find(lines(_).contains('E'))
    val n = lines(m.get).indexOf('E')
    Node(None, 'z', (m.get, n), 0)
  }

  def part1(trueLines: Vector[String]): Int = {
    val endNode = getEndNode(trueLines)
    val startNode = getStartNode(trueLines)
    val frontier = new Frontier(endNode)
    val lines = trueLines.map(_.map(el => if (el == 'E') endNode.elevation else if (el == 'S') startNode.elevation else el))
    startNode.availableActions(lines).foreach(frontier.insert)
    while (!frontier.isEmpty) {
      val node = frontier.pop()
      if (node.coords == endNode.coords) {
        return node.g
      }
      node.availableActions(lines).foreach(frontier.insert)
    }
    -1
  }

  def allAs(lines: Vector[String], endNode: Node): Vector[Node] = {
    lines.indices.toVector
      .flatMap(m => lines(m).indices.map(n => Node(None, lines(m)(n), (m, n), 0)))
      .filter(_.elevation == 'a')
      .sortBy(_.f(endNode))
  }

  def part2(trueLines: Vector[String]): Int = {
    val endNode = getEndNode(trueLines)
    val allStarts: Vector[(Int, Int)] = allAs(trueLines, endNode).map(_.coords)
    val lines: Vector[String] = trueLines.map(ln => ln.map(el => if (el == 'E') 'z' else if (el == 'S') 'a' else el).mkString)
    val frontier = new Frontier(endNode)
    endNode.availableActionsPt2(lines).foreach(frontier.insert)
    while (!frontier.isEmpty) {
      val node = frontier.pop()
      print(s"${node.elevation} nodes in frontier ${frontier.nodes.length}\r")
      if (allStarts.contains(node.coords)) {
        println()
        return node.g
      } else {
        node.availableActionsPt2(lines).foreach(frontier.insert)
      }
    }
    println()
    -1
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day12/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    //    println(s"Part1: ${part1(lines)}")
    println(s"Part2: ${part2(lines)}")

  }

}
