import scala.io.Source
import scala.util.matching.Regex

object Day7 {

  trait Loc {
    def space(): Long
  }

  case class File(size: Long, name: String, path: List[String]) extends Loc {
    override def space(): Long = size
  }

  case class Dir(name: List[String]) extends Loc {
    var subLocs: List[Loc] = List()

    override def space(): Long = {
      subLocs.map(_.space()).sum
    }

    def addLoc(l: Loc): Unit = {
      subLocs = l :: subLocs
    }
  }

  val cdPatternDown: Regex = "^\\$ cd ([/\\w]+)".r
  val cdPatternUp: Regex = "^(\\$ cd \\.\\.)$".r
  val lsPattern: Regex = "(^\\$ ls.*$)".r
  val cmdPattern: Regex = "(^\\$.*)".r
  val lsFilePattern: Regex = "^(\\d+ [.\\w]+)$".r
  val lsDirPattern: Regex = "^dir (\\S+)$".r

  def part1(lines: Iterator[String]): Long = {
    var tree: Map[List[String], Dir] = Map()
    var cwd: List[String] = List()
    lines.foreach {
      case cdPatternDown(m) => {
        cwd = m :: cwd
        if (!tree.contains(cwd)) tree = Map(cwd -> Dir(cwd)) ++ tree
      }
      case cdPatternUp(_) => {
        cwd = cwd.tail
      }
      case lsPattern(_) =>
      case lsFilePattern(nnln) => {
        val sp = nnln.split(" ")
        tree(cwd).addLoc(File(sp(0).toLong, sp(1), cwd))
      }
      case lsDirPattern(name) => {
        tree(cwd).addLoc({
          if (tree.contains(name :: cwd)) tree(name :: cwd) else {
            val locCwd = name :: cwd
            tree = Map(locCwd -> Dir(locCwd)) ++ tree
            tree(locCwd)
          }
        })
      }
    }

    tree.values.map(_.space()).filter(_ <= 100000L).sum
  }

  def part2(lines: Iterator[String]): Long = {
    var tree: Map[List[String], Dir] = Map()
    var cwd: List[String] = List()
    lines.foreach {
      case cdPatternDown(m) => {
        cwd = m :: cwd
        if (!tree.contains(cwd)) tree = Map(cwd -> Dir(cwd)) ++ tree
      }
      case cdPatternUp(_) => {
        cwd = cwd.tail
      }
      case lsPattern(_) =>
      case lsFilePattern(nnln) => {
        val sp = nnln.split(" ")
        tree(cwd).addLoc(File(sp(0).toLong, sp(1), cwd))
      }
      case lsDirPattern(name) => {
        tree(cwd).addLoc({
          if (tree.contains(name :: cwd)) tree(name :: cwd) else {
            val locCwd = name :: cwd
            tree = Map(locCwd -> Dir(locCwd)) ++ tree
            tree(locCwd)
          }
        })
      }
    }
    val avail = 70000000L - tree(List("/")).space()
    val required = 30000000L
    tree.values.map(_.space()).filter(_ >= (required - avail)).toList.min
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day7/actual.txt")
    val (p1iter, p2iter) = src.getLines().iterator.duplicate
    println(s"Part1: ${part1(p1iter)}")
    println(s"Part2: ${part2(p2iter)}")
  }

}
