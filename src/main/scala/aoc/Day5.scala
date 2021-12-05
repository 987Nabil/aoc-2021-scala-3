package aoc

@main
def day5(): Unit =
  val Lines = """([0-9]*),([0-9]*)\s*->\s*([0-9]*),([0-9]*)""".r
  val lines =
    readResourceLines("day5.txt")
      .map { case Lines(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt)) }

  val linesHV = lines.filter { case (start, end) => start._1 == end._1 || start._2 == end._2 }

  val coveredHVPoints =
    linesHV.flatMap { case (start, end) =>
      for
        x <- start._1.min(end._1) to start._1.max(end._1)
        y <- start._2.min(end._2) to start._2.max(end._2)
      yield (x, y)
    }

  val linesD = lines.diff(linesHV)

  val coveredDPoints =
    linesD.flatMap { case (start, end) =>
      val xRange = if start._1 > end._1 then Range.inclusive(start._1, end._1, -1) else start._1 to end._1
      val yRange = if start._2 > end._2 then Range.inclusive(start._2, end._2, -1) else start._2 to end._2
      xRange zip yRange
    }

  val part1 =
    coveredHVPoints.groupMapReduce(identity)(_ => 1)(_ + _).count { case (_, i) => i >= 2 }

  val part2 =
    (coveredDPoints ++ coveredHVPoints)
      .groupMapReduce(identity)(_ => 1)(_ + _).count { case (_, i) => i >= 2 }

  println(s"part 1: $part1")
  println(s"part 2: $part2")
