import scala.annotation.tailrec
import scala.io.Source

object Day14 {
  def wall(input: String): Set[(Int, Int)] = {
    val corners = input.split("->").toList.map(_.trim).map(coords => {
      val cs = coords.split(",").map(_.toInt)
      (cs.head, cs.tail.head)
    })
    corners.sliding(2).flatMap(cs => expandWall(cs.head, cs.tail)).toSet
  }

  @tailrec
  def expandWall(endPoint: (Int, Int), coordsList: List[(Int, Int)]): List[(Int, Int)] = {
    if (coordsList.contains(endPoint)) coordsList else {
      val diff: (Int, Int) = (endPoint._1 - coordsList.head._1, endPoint._2 - coordsList.head._2)
      diff match {
        case (0, value) =>
          if (value > 0)
            expandWall(endPoint, (endPoint._1, coordsList.head._2 + 1) :: coordsList)
          else
            expandWall(endPoint, (endPoint._1, coordsList.head._2 - 1) :: coordsList)
        case (value, 0) =>
          if (value > 0)
            expandWall(endPoint, (coordsList.head._1 + 1, endPoint._2) :: coordsList)
          else
            expandWall(endPoint, (coordsList.head._1 - 1, endPoint._2) :: coordsList)
      }
    }
  }

  @tailrec
  def dropSand(colNum: Int, yStart: Int, takenLocations: Set[(Int, Int)], cDims: CaveDimensions): Set[(Int, Int)] = {
    val takeDirection: (String, Option[Int]) = {
      if (colNum == cDims.xMin)
        ("ABYSS", None)
      else if (colNum == cDims.xMax)
        ("ABYSS", None)
      else {
        val coordsTakenInDropRow = takenLocations.filter(p => p._1 == colNum).filter(p => p._2 > yStart)
        val yHit = coordsTakenInDropRow.map(_._2).min - 1
        if (!takenLocations.contains(colNum - 1, yHit + 1))
          ("LEFT", Some(yHit))
        else if (!takenLocations.contains(colNum + 1, yHit + 1))
          ("RIGHT", Some(yHit))
        else
          ("SETTLED", Some(yHit))
      }
    }
    takeDirection match {
      case ("ABYSS", None) => takenLocations
      case ("LEFT", Some(yHit)) => dropSand(colNum - 1, yHit + 1, takenLocations, cDims)
      case ("RIGHT", Some(yHit)) => dropSand(colNum + 1, yHit + 1, takenLocations, cDims)
      case ("SETTLED", Some(yHit)) => ((colNum, yHit) :: takenLocations.toList).toSet
    }
  }

  case class CaveDimensions(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  def getDimensions(walls: Iterable[(Int, Int)]): CaveDimensions = {
    val xs = walls.map(_._1)
    val ys = walls.map(_._2)
    CaveDimensions(xs.min, xs.max, 0, ys.max)
  }

  var initialLocationCount = 0
  @tailrec
  def keepDroppingSandUntilNoChange(dropPoint: (Int, Int), takenLocations: Set[(Int, Int)], cDims: CaveDimensions): Set[(Int, Int)] = {
    val newLocations = dropSand(dropPoint._1, dropPoint._2, takenLocations, cDims)
    print(s"sand dropped: ${newLocations.size - initialLocationCount} \r")
    if (newLocations == takenLocations || newLocations.contains(dropPoint))
      newLocations
    else
      keepDroppingSandUntilNoChange(dropPoint, newLocations, cDims)
  }

  def drawRow(locations: Set[Int], cDims: CaveDimensions): Unit = {
    for {
      pixel <- cDims.xMin to cDims.xMax
    } if (locations.contains(pixel)) print("#") else print(".")
    println()
  }
  def drawLocations(locations: Set[(Int, Int)], cDims: CaveDimensions): Unit = {
    for {
      row <- cDims.yMin to cDims.yMax
    } drawRow(locations.filter(_._2 == row).map(_._1), cDims)
  }

  def part1(lines: List[String]): Int = {
    val walls = lines.flatMap(ln => wall(ln)).toSet
    val cDims = getDimensions(walls)
    initialLocationCount = walls.size
    val finalTakenLocations = keepDroppingSandUntilNoChange((500,0), walls, cDims)
    println()
    finalTakenLocations.size - walls.size
  }

  def part2(lines: List[String]): Int = {
    val wallsTmp = lines.flatMap(ln => wall(ln)).toSet
    val cDimsTmp = getDimensions(wallsTmp)
    val floorWidth = (cDimsTmp.yMax+3)*2
    val floorCorners = ((500-(floorWidth/2+1), cDimsTmp.yMax+2), (500+(floorWidth/2+1), cDimsTmp.yMax+2))
    val newLines = s"${floorCorners._1._1},${floorCorners._1._2} -> ${floorCorners._2._1},${floorCorners._2._2}" :: lines
    val walls = newLines.flatMap(ln => wall(ln)).toSet
    val cDims = getDimensions(walls)
    initialLocationCount = walls.size
    drawLocations(walls, cDims)
    val finalTakenLocations = keepDroppingSandUntilNoChange((500,0), walls, cDims)
    println()
    drawLocations(finalTakenLocations, cDims)
    finalTakenLocations.size - walls.size
  }

  def main(args: Array[String]): Unit = {
    val src = Source.fromFile("input/day14/actual.txt")
    val lines = src.getLines().toList
    src.close()
    println(s"Part1: ${part1(lines)}")
    println(s"Part2: ${part2(lines)}")
  }
}
