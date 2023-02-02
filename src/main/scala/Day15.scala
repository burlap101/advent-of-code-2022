import scala.annotation.tailrec
import scala.io.Source

object Day15 {

  trait Position {
    val loc: (Long, Long)
  }

  case class BoundsVector(p1: (Long, Long), p2: (Long, Long)) {
    def y(x: Long): Option[Long] = m() match {
      case Some(mValue) => b() match {
        case Some(bValue) => Some(mValue * x + bValue)
        case None => None
      }
      case None => None
    }
    def x(y: Long): Option[Long] = m() match {
      case Some(mValue) => b() match {
        case Some(bValue) => (y - bValue)/mValue
        case None => 0
      }
      case None => b() match {
        case Some(_) => Some(0)
        case None => None
      }
    }
    def m(): Option[Long] = (p1._1 - p1._2) match {
      case 0 => None
      case _ => Some((p1._2 - p2._2) / (p1._1 - p1._2))
    }
    def b(): Option[Long] = m() match {
      case Some(value) => Some(p1._2 - value * p1._1)
      case None => None
    }
    def intersects(that: BoundsVector): Option[(Long, Long)] = {
      val x = (that.b() - this.b())/(this.m()-that.m())
      if (this.y(x) == that.y(x)) Some((this.y(x), x)) else None
    }
  }

  case class Sensor(loc: (Long, Long), beacon: Beacon, xyMax: Long = 20L) extends Position {
    def distance(): Long = {
      math.abs(loc._1 - beacon.loc._1) + math.abs(loc._2 - beacon.loc._2)
    }
    def contains(poi: (Long, Long)): Boolean = {
      math.abs(loc._1 - poi._1) + math.abs(loc._2 - poi._2) <= distance()
    }
    def isOnEdge(poi: (Long, Long)): Boolean = {
      math.abs(loc._1 - poi._1) + math.abs(loc._2 - poi._2) == distance()
    }
    def nextPointAlongOutside(prev: (Long, Long)): Option[(Long, Long)] = {
      val dist = math.abs(prev._1 - loc._1) + math.abs(prev._2 - loc._2)
      if (dist != distance()+1) {
        None
      } else {
        val diff = (prev._1 - loc._1, prev._2 - loc._2)
        val contender = diff match {
          case (0, l) => if (l > 0) (prev._1 + 1, prev._2 - 1) else (prev._1 - 1, prev._2 + 1)
          case (l, 0) => if (l > 0) (prev._1 - 1, prev._2 - 1) else (prev._1 + 1, prev._2 + 1)
          case (l1, l2) =>
            if (l1 > 0 && l2 > 0)
              (prev._1 + 1, prev._2 - 1)
            else if (l1 > 0 && l2 < 0)
              (prev._1 - 1, prev._2 - 1)
            else if (l1 < 0 && l2 < 0)
              (prev._1 - 1, prev._2 + 1)
            else
              (prev._1 + 1, prev._2 + 1)
        }
        contender match {
          case (x, y) if x == loc._1 && y > loc._2 => None
//          case (x, y) if x > xyMax && 2*loc._2 - y > 0 => Some(xyMax, 2*loc._2 - y)
//          case (x, y) if y > xyMax && 2*loc._1 - x < xyMax => Some(2*loc._1 - x,  xyMax)
//          case (x, y) if x < 0 && 2*loc._2 - y < xyMax => Some(0, 2*loc._2 - y)
//          case (x, y) if y < 0 && 2*loc._1 - x > 0 => Some(2*loc._1 - x, 0)
          case (x, y) if x > xyMax || y > xyMax || x < 0 || y < 0 => nextPointAlongOutside(contender)
          case _ => Some(contender)
        }
      }
    }
    def firstPoint(): Option[(Long, Long)] = {
      val top = (loc._1, loc._2 + distance() + 1)
      if (top._1 > xyMax || top._2 > xyMax || top._1 < 0 || top._2 < 0)
        nextPointAlongOutside(top)
      else
        Some(top)
    }
    def vectors(): List[BoundsVector] = {
      val top = (loc._1, distance() + loc._2)
      val right = (loc._1 + distance(), loc._2)
      val xMaxBv = BoundsVector((xyMax,0), (xyMax, xyMax))
      val xAxisBv = BoundsVector((0, 0), (0, xyMax))
      val yMaxBv = BoundsVector((0, xyMax), (xyMax, xyMax))
      val yAxisBv = BoundsVector((0,0), (xyMax, 0))
      val bv = BoundsVector(top, right).intersects(xMaxBv) match {
        case Some(point) => if (isOnEdge(point)) BoundsVector(top, point) else BoundsVector(top, right)
        case None => BoundsVector(top, right)
      }
    }
  }

  case class Beacon(loc: (Long, Long)) extends Position

  def extractSensors(lines: List[String], xyMax: Long = 0): List[Sensor] = {
    val coordsRE = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    lines.map {
      case coordsRE(sx, sy, bx, by) => Sensor((sx.toLong, sy.toLong), Beacon((bx.toLong, by.toLong)), xyMax)
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

  def isBlank(poi: (Long, Long))(implicit sensors: List[Sensor]): Boolean = {
    !sensors.map(_.contains(poi)).reduceLeft(_ || _)
  }

  @tailrec
  def findBlank(sensor: Sensor, poi: (Long, Long))(implicit sensors: List[Sensor]): Option[(Long, Long)] = {
    print(s"exploring Sensor:${sensors.indexOf(sensor) + 1}; poi: $poi           \r")
    sensor.firstPoint() match {
      case poi => None
      case _ =>
        if (isBlank(poi))
          Some(poi)
        else
          findBlank(sensor, sensor.nextPointAlongOutside(poi).head)
    }
  }


  def part2(lines: List[String], xyMax: Long): String = {
    implicit val sensors: List[Sensor] = extractSensors(lines, xyMax)
    val tops = sensors.flatMap(_.firstPoint()).filter(isBlank)
    if (tops.nonEmpty)
      s"${tops.head}"
    else {
      var blankPoint: Option[(Long, Long)] = None
      val res = sensors.find(s => {
        findBlank(s, (s.loc._1 + 1, s.loc._2 + s.distance())) match {
          case Some(value) =>
            blankPoint = Some(value)
            true
          case None => false
        }
      })
      println()
      s"$res"
    }
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day15/actual.txt")
    val lines = src.getLines().toList
    src.close()
    println(s"Part 1: ${part1(lines, 2000000)}")
  }
}
