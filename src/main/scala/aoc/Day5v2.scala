package aoc

case class Point(x: Int, y: Int)
case class Line(start: Point, end: Point):
  lazy val isVertical: Boolean    = start.y == end.y
  lazy val isHorizontal: Boolean  = start.x == end.x
  lazy val isDiagonal: Boolean    = !isVertical && !isHorizontal
  lazy val linePoints: Seq[Point] = if isDiagonal then diagonalPoints else nonDiagonalPoints

  private lazy val diagonalPoints: Seq[Point] =
    (coordinateRange(start.x, end.x) zip coordinateRange(start.y, end.y)).map(Point.apply)

  def coordinateRange(start: Int, end: Int): Range.Inclusive =
    if start > end then Range.inclusive(start, end, -1) else start to end

  private lazy val nonDiagonalPoints: Seq[Point] = {
    for
      x <- coordinateRange(start.x, end.x)
      y <- coordinateRange(start.y, end.y)
    yield (x, y)
  }.map(Point.apply)

object Line:
  private val LineExtractor          = """([0-9]*),([0-9]*)\s*->\s*([0-9]*),([0-9]*)""".r
  def fromString(line: String): Line =
    line match
      case LineExtractor(x1, y1, x2, y2) =>
        Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

@main
def day5v2(): Unit =

  val lines = readResourceLines("day5.txt").map(Line.fromString)

  val (linesHV, linesD) = lines.partition(l => l.isVertical || l.isHorizontal)

  println(s"part 1: ${dangerousPointsCount(coveredPoints(linesHV))}")
  println(s"part 2: ${dangerousPointsCount(coveredPoints(lines))}")

def dangerousPointsCount(points: Seq[Point]): Int =
  points.groupMapReduce(identity)(_ => 1)(_ + _).count { case (_, i) => i >= 2 }

def coveredPoints(points: Seq[Line]): Seq[Point] =
  points.flatMap(_.linePoints)
