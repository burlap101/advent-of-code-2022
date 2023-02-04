import scala.annotation.tailrec
import scala.io.Source

object Day15 {

  trait Position {
    val loc: (Long, Long)
  }

  case class Sensor(loc: (Long, Long), beacon: Beacon) extends Position {
    def distance(): Long = {
      math.abs(loc._1 - beacon.loc._1) + math.abs(loc._2 - beacon.loc._2)
    }
  }

  case class Beacon(loc: (Long, Long)) extends Position

  def extractSensors(lines: List[String]): List[Sensor] = {
    val coordsRE = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    lines.map {
      case coordsRE(sx, sy, bx, by) => Sensor((sx.toLong, sy.toLong), Beacon((bx.toLong, by.toLong)))
    }
  }

  case class CaveDimensions(xMin: Long, xMax: Long, yMin: Long, yMax: Long)

  def caveDimensions(sensors: List[Sensor]): CaveDimensions = {
    val xs = sensors.flatMap[Long](s => List(s.loc._1, s.beacon.loc._1))
    val ys = sensors.flatMap[Long](s => List(s.loc._2, s.beacon.loc._2))
    CaveDimensions(xs.min, xs.max, ys.min, ys.max)
  }

  case class RowSpan(start: Long, end: Long) {
    def contains(i: Long): Boolean = start <= i || i <= end
    def join(that: RowSpan): Option[RowSpan] = {
      if (this.contains(that.start) || this.contains(that.end)) {
        Some(
          RowSpan(
            List(this.start, that.start).min,
            List(this.end, that.end).max,
          )
        )
      } else None
    }
  }

  def calcCoverageForRow(roi: Long, sensors: List[Sensor]): Long = {
    val beaconSet: Set[Long] = sensors.filter(_.beacon.loc._2 == roi).map(_.beacon.loc._1).toSet
    val spans: List[RowSpan] = sensors.map(s => {
      val center = s.loc._1
      val distanceFromRow = math.abs(s.loc._2 - roi)
      val countHalf = s.distance() - distanceFromRow
      RowSpan(center - countHalf, center + countHalf)
    })
    val reducedSpans = spanJoinReduce(spans.head::Nil, spans.tail)
    reducedSpans.map(s => s.end + 1 - s.start).sum - beaconSet.count(b => { reducedSpans.exists(r => r.contains(b)) })
  }

  @tailrec
  def spanJoinReduce(acc: List[RowSpan], todo: List[RowSpan]): List[RowSpan] = {
    if (todo == Nil) acc else {
      acc.head.join(todo.head) match {
        case Some(span) => spanJoinReduce(span :: acc.tail, todo.tail)
        case None => spanJoinReduce(acc.head :: todo.head :: acc.tail, todo.tail)
      }
    }
  }


  def part1(lines: List[String], roi: Long): Long = {
    val sensors = extractSensors(lines)
    val withinDistance = sensors
      .filter(s => {
        (s.loc._2 <= roi && roi <= s.loc._2 + s.distance()) || (s.loc._2 >= roi && roi >= s.loc._2 - s.distance())
      })
    calcCoverageForRow(roi, withinDistance)
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day15/actual.txt")
    val lines = src.getLines().toList
    src.close()
    println(s"Part 1: ${part1(lines, 2000000)}")
  }
}
