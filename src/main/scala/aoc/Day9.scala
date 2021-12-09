package aoc

import scala.util.chaining.*

@main
def day9(): Unit =
  val map = readResourceLines("day9.txt").map(_.grouped(1).toSeq.map(_.toInt))

  def maybeAdjacent(x: Int, y: Int) =
    map.lift(y).flatMap(_.lift(x)).map((x, y) -> _)

  def adjacentPoints(x: Int, y: Int) =
    List(
      maybeAdjacent(x, y - 1),
      maybeAdjacent(x, y + 1),
      maybeAdjacent(x - 1, y),
      maybeAdjacent(x + 1, y)
    ).flatten.toMap

  val lowPoints =
    for
      x <- map.head.indices
      y <- map.indices
    yield
      val point = map(y)(x)
      if adjacentPoints(x, y).values.min <= point then None
      else Some(adjacentPoints(x, y) + ((x, y) -> point) -> point)
    end for
  .flatten.toMap

  def basin(adjPoints: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    adjPoints.foldLeft(adjPoints.filterNot(_._2 == 9)) { case (acc, ((x, y), value)) =>
      acc ++ basin(adjacentPoints(x, y).filter(_._2 > value))
    }

  val basins =
    for (adjPoints, lowPoint) <- lowPoints
    yield basin(adjPoints)

  println(s"part 1: ${lowPoints.values.sum + lowPoints.size}")

  println(s"part 2: ${basins.map(_.size).toList.sorted.takeRight(3).product}")
