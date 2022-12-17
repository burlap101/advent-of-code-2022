import scala.annotation.tailrec
import scala.io.Source

object Day8 {
  case class Tree(height: Int, var isVisible: Boolean)
  def getGrid(lines: Iterable[String]): Vector[Vector[Int]] = lines.toVector.map(_.toVector.map(_.asDigit))
  def getTrees(grid: Vector[Vector[Int]]): Map[(Int, Int), Tree] = {
    var result: Map[(Int, Int), Tree] = Map()
    for {
      m <- grid.indices
      n <- grid(0).indices
    } {
      if (m==0 || m==grid.length-1 || n==0 || n==grid(0).length-1) {
        result += (m, n) -> Tree(grid(m)(n), isVisible = true)
      } else {
        result += (m, n) -> Tree(grid(m)(n), isVisible = false)
      }
    }
    result
  }

  def determineVisibility(trees: Vector[Tree]): Unit = {
    val tallestHeight = trees.maxBy(_.height).height
    val firstIdx = trees.indexWhere(_.height == tallestHeight)
    val lastIdx = trees.lastIndexWhere(_.height == tallestHeight)
    trees(firstIdx).isVisible = true
    trees(lastIdx).isVisible = true
    if (firstIdx != 0) {
      val sub1 = trees.slice(0, firstIdx)
      workFromTallest(sub1)
    }
    if (lastIdx != trees.length-1) {
      val sub2 = trees.slice(lastIdx + 1, trees.length).reverse
      workFromTallest(sub2)
    }
  }

  @tailrec
  def workFromTallest(trees: Vector[Tree]): Unit = {
    if (trees.nonEmpty) {
      val tallestHeight = trees.maxBy(_.height).height
      val firstIdx = trees.indexWhere(_.height == tallestHeight)
      trees(firstIdx).isVisible = true
      workFromTallest(trees.slice(0,firstIdx))
    } else if (trees.length == 1) {
      trees(0).isVisible = true
    }
  }

  def part1(lines: Iterable[String]): Int = {
    val trees = getTrees(getGrid(lines))
    trees.groupBy(k => k._1._1).foreach(row => {
      val sortedKeys = row._2.keys.toList.sortBy(_._2)
      val rowTrees = sortedKeys.map(k => row._2(k))
      determineVisibility(rowTrees.toVector)
    })
    trees.groupBy(k => k._1._2).foreach(col => {
      val sortedKeys = col._2.keys.toList.sortBy(_._1)
      val colTrees = sortedKeys.map(k => col._2(k))
      determineVisibility(colTrees.toVector)
    })
    trees.count(_._2.isVisible)
  }

  def calcViewScore(tree: Tree, views: Vector[Vector[Tree]]): Int = {
    views.map(ts => {
      val s = ts.takeWhile(_.height < tree.height).length
      if (s == ts.length) s else s+1
    }).product
  }

  def calcViewScoreV2(coords: (Int, Int), tree: Tree, views: Vector[Vector[Tree]]): Int = {
    val result = views.map(ts => {
      val s = ts.takeWhile(_.height < tree.height).length
      if (s == ts.length) s else s+1
    }).product
    if (result > 600000) {
      println(s"$coords = $result")
      println(tree)
      views.foreach(v => println(v.map(_.height).mkString(","), s"length = ${v.length}"))
    }
    result
  }

  def part2(lines: Iterable[String]): Int = {
    val trees = getTrees(getGrid(lines))
    val gridDim: Int = lines.size
    trees
      .filter(p => p._1._1 > 0 && p._1._1 < gridDim-1 && p._1._2 > 0 && p._1._2 < gridDim - 1)
      .map(t => {
        val rowTmp = trees.filter(_._1._1 == t._1._1).keys.toVector.sortBy(_._2)
        val (ri1, ri2) = rowTmp.splitAt(rowTmp.indexOf(t._1))
        val colTmp = trees.filter(_._1._2 == t._1._2).keys.toVector.sortBy(_._1)
        val (ci1, ci2) = colTmp.splitAt(colTmp.indexOf(t._1))
        val (r1, r2) = (ri1.map(k => trees(k)), ri2.map(k => trees(k)))
        val (c1, c2) = (ci1.map(k => trees(k)), ci2.map(k => trees(k)))
//        if (t._1 == (3,2)) println(r1.reverse.map(_.height), r2.map(_.height), c1.reverse.map(_.height), c2.map(_.height))
        calcViewScoreV2(t._1, t._2, Vector(r1.reverse, r2.tail, c1.reverse, c2.tail))
      })
      .max
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day8/actual.txt")
    val lines = src.getLines().toVector
    src.close()
    println(s"Part1: ${part1(lines)}")
    println(s"Part2: ${part2(lines)}")
  }
}
