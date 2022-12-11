import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day7Test extends AnyFunSuite {
  test("Day7.part1") {
    val src = Source.fromFile("input/day7/test.txt")
    val lines = src.getLines().iterator
    assert(Day7.part1(lines) == 95437)
    src.close()
  }
  test("lsPattern") {
    val lines =
     """|$ ls
        |dir e
        |1123 doc.txt
        |$ noop
        |$ noop
        |$ noop
        |""".stripMargin
    val iter: Iterator[String] = lines.split("\n").iterator
    var returned: List[String] = List()
    var f:Day7.File = Day7.File(0L, "", List(""))
    iter.foreach {
      case Day7.lsPattern(_) => {
        iter.takeWhile(p => !Day7.cmdPattern.matches(p)).foreach {
          case Day7.lsFilePattern(mm) => {
            returned = mm :: returned
            val sp = mm.split(" ")
            f = Day7.File(sp(0).toLong, sp(1), List("somedir"))
          }
          case Day7.lsDirPattern(mm) => returned = mm :: returned
          case Day7.cmdPattern(cmd) => println(s"inner loop: $cmd")
        }
      }
      case Day7.cmdPattern(cmd) => println(cmd)
    }
//    assert(returned == List("$ ls"))
    assert(returned == List("1123 doc.txt", "e"))
    assert(f == Day7.File(1123L, "doc.txt", List("somedir")))
  }

  test("cdPatterns") {
   val lines =
    """|$ cd /
       |$ cd e
       |$ cd f
       |""".stripMargin
    val iter: Iterator[String] = lines.split("\n").iterator
    var cwd: List[String] = List()
    iter.foreach {
      case Day7.cdPatternDown(v) => cwd = v :: cwd
      case Day7.cdPatternUp(_) => cwd = cwd.tail
    }
    assert(cwd == List("f","e","/"))
  }
  test("cdPatterns2") {
    val lines =
     """|$ cd /
        |$ cd gwnwqcgq
        |""".stripMargin
    val iter: Iterator[String] = lines.split("\n").iterator
    var cwd: List[String] = List()
    iter.foreach {
      case Day7.cdPatternUp(_) => cwd = cwd.tail
      case Day7.cdPatternDown(v) => cwd = v :: cwd
    }
    assert(cwd == List("gwnwqcgq", "/"))
  }

  test("Map with List as key") {
    val l = List("a", "b")
    var m: Map[List[String], Int] = Map()
    m = Map(("c"::l) -> 123) ++ m
    val l2 = "c"::l
    assert(m(l2) == 123)
    assert(m.contains(l2))
  }
  test("cd regex") {
    val sdown = "$ cd a"
    val sroot = "$ cd /"
    val sup = "$ cd .."
    assert(Day7.cdPatternDown.matches(sdown))
    assert(!Day7.cdPatternDown.matches(sup))
    assert(Day7.cdPatternDown.matches(sroot))
    assert(Day7.cdPatternUp.matches(sup))
  }
  test("test addLoc") {
    val f = Day7.File(1L, "somefile", List("somedir"))
    val f2 = Day7.File(2L, "anotherFile", List("somedir"))
    val dir = Day7.Dir(List("somedir"))
    dir.addLoc(f)
    dir.addLoc(f2)
    assert(dir.space() == 3L)
  }
  test("Day7.part2") {
    val src = Source.fromFile("input/day7/test.txt")
    val lines = src.getLines().iterator
    assert(Day7.part2(lines) == 24933642)
    src.close()
  }
}
