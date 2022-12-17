import Day8.{Tree, calcViewScore}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap
import scala.io.Source

class Day8Test extends AnyFunSuite {
  test("Day8.getGrid") {
    val sg =
     """|123
        |456
        |""".stripMargin
    assert(Day8.getGrid(sg.split("\n")) == Vector(Vector(1,2,3),Vector(4,5,6)))
  }
  test("Groupby Tuple2") {
    val ts = Vector((1,1), (1,2), (2,1), (2,2))
    assert(ts.groupBy(t => t._2) == Map(
      1 -> Vector((1,1), (2,1)),
      2 -> Vector((1,2), (2,2)),
    ))
  }
  test("Day8.getTrees") {
    val grid = Vector(Vector(1,2), Vector(3,4))
    assert(Day8.getTrees(grid) == Map(
      (0,0) -> Day8.Tree(1, isVisible = true),
      (0,1) -> Day8.Tree(2, isVisible = true),
      (1,0) -> Day8.Tree(3, isVisible = true),
      (1,1) -> Day8.Tree(4, isVisible = true),
    ))
  }
  test("Day8.workFromTallest") {
    val heights = Vector(1, 2, 1, 2, 3)
    val trees = heights.map(h => Day8.Tree(h, isVisible = false))
    val expected = Vector(true, true, false, false, true)
    Day8.workFromTallest(trees)
    assert(trees.map(_.isVisible) == expected)
  }
  test("Day8.determineVisibility") {
    val heights = Vector(1, 2, 1, 2, 3, 2, 1)
    val trees = heights.map(h => Day8.Tree(h, isVisible = false))
    val expected = Vector(true, true, false, false, true, true, true)
    Day8.determineVisibility(trees)
    assert(trees.map(_.isVisible) == expected)
  }
  test("Day8.part1") {
    val src = Source.fromFile("input/day8/test.txt")
    val lines = src.getLines().toVector
    val result = Day8.part1(lines)
    src.close()
    assert(result == 21)
  }
  test("Day8.determineVisibility from test.txt") {
    val src = Source.fromFile("input/day8/test.txt")
    val lines = src.getLines().toVector
    val trees = Day8.getTrees(Day8.getGrid(lines))
    println(trees)
    assert(trees.size == 25)
  }
  test("Day8.part2") {
    val src = Source.fromFile("input/day8/test.txt")
    val lines = src.getLines().toVector
    val result = Day8.part2(lines)
    src.close()
    assert(result == 8)
  }
  test("Day8.viewScore") {
    val v1 = Vector(3, 5, 3).map(Tree(_, isVisible = false))
    val v2 = Vector(3).map(Tree(_, isVisible = false))
    val v3 = Vector(5, 2).map(Tree(_, isVisible = false))
    val v4 = Vector(1, 2).map(Tree(_, isVisible = false))
    assert(calcViewScore(Tree(5, isVisible = false), Vector(v1, v2, v3, v4)) == 4)
  }
  test("Day8.part2filter") {
    val src = Source.fromFile("input/day8/test.txt")
    val lines = src.getLines().toVector
    val trees = Day8.getTrees(Day8.getGrid(lines))
    val gridDim = 5
    assert(trees.count(p => p._1._1 > 0 && p._1._1 < gridDim - 1 && p._1._2 > 0 && p._1._2 < gridDim - 1) == 9)
  }
  test("Day8.part2map") {
    val src = Source.fromFile("input/day8/test.txt")
    val lines = src.getLines().toVector
    val trees = Day8.getTrees(Day8.getGrid(lines))
    val gridDim = 5
    val res = trees
      .filter(p => p._1._1 > 0 && p._1._1 < gridDim-1 && p._1._2 > 0 && p._1._2 < gridDim - 1)
      .map(t => {
        val row = trees.filter(_._1._1 == t._1._1).keys.toList.sortBy(_._2).map(k => trees(k)).toVector
        val col = trees.filter(_._1._2 == t._1._2).keys.toList.sortBy(_._1).map(k => trees(k)).toVector
        val (r1, r2) = row.splitAt(row.indexOf(t._2))
        val (c1, c2) = col.splitAt(col.indexOf(t._2))
        calcViewScore(t._2, Vector(r1.reverse, r2, c1.reverse, c2))
      })
    assert(res.size == 9)
  }
  test("Day8.gridDim") {
    val src = Source.fromFile("input/day8/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    assert(lines.size == 99)
  }
  test("handcalc") {
    assert(58*40*20*15 == 696000)
  }
  test("indexing") {
    val v = 99 - 84
    assert(v == 20)
  }
}
