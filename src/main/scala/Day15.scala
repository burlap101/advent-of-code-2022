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

  def calcCoverageForRow(roi: Long, sensors: List[Sensor]): Set[Long] = {
    val beaconSet = sensors.filter(_.beacon.loc._2 == roi).map(_.beacon.loc._1).toSet
    sensors.flatMap(s => {
      val center = s.loc._1
      val distanceFromRow = math.abs(s.loc._2 - roi)
      val countHalf = s.distance() - distanceFromRow
      ((center - countHalf) to (center + countHalf)).toSet
    }).toSet.diff(beaconSet)
  }

  def part1(lines: List[String], roi: Long): Long = {
    val sensors = extractSensors(lines)
    val withinDistance = sensors
      .filter(s => {
        (s.loc._2 <= roi && roi <= s.loc._2 + s.distance()) || (s.loc._2 >= roi && roi >= s.loc._2 - s.distance())
      })
    val coverage = calcCoverageForRow(roi, withinDistance)
    coverage.size
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day15/actual.txt")
    val lines = src.getLines().toList
    src.close()
    println(s"Part 1: ${part1(lines, 2000000)}")
  }
}
