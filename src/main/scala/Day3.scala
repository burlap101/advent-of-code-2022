import scala.io.Source

object Day3 {
  val alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def part1(filename: String): Long = {
    var accum: Long = 0L
    val reader = Source.fromFile(filename)
    reader.getLines().foreach(ln => {
      val rsac = (ln.slice(0, ln.length/2), ln.slice(ln.length/2, ln.length))
      val anom = rsac._1.filter(c => rsac._2.contains(c)).charAt(0)
      accum += alphabet.indexOf(anom) + 1
    })
    reader.close()
    accum
  }


  def part2(filename: String): Long = {
    var accum: Long = 0L
    val reader = Source.fromFile(filename)
    val lines = reader.getLines().toList
    for {
      i <- 0 to lines.length-3 by 3
    } {
      val sacs = (lines(i), lines(i+1), lines(i+2))
      val common = sacs._1.filter(c => {
        sacs._2.contains(c)
      }).filter(c => {
        sacs._3.contains(c)
      })
      accum += alphabet.indexOf(common(0))+1
    }
    accum
  }

  def main(args: Array[String]): Unit = {
    println(s"Part1: ${part1("input/day3/actual.txt")}")
    println(s"Part2: ${part2("input/day3/actual.txt")}")
  }

}
