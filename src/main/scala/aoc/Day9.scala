package aoc

import scala.annotation.tailrec
import scala.util.chaining.*

@main
def day9(): Unit =
  val map = readResourceLines("day9.txt").map(_.grouped(1).toSeq.map(_.toInt))

  def maybeAdjacent(y: Int, x: Int) =
    map.lift(y).flatMap(_.lift(x)).map(_ => (y, x))

  def adjacentPoints(y: Int, x: Int): Set[(Int, Int)] =
    Set(
      maybeAdjacent(y - 1, x),
      maybeAdjacent(y + 1, x),
      maybeAdjacent(y, x - 1),
      maybeAdjacent(y, x + 1)
    ).flatten

  val lowPoints =
    for
      x <- map.head.indices
      y <- map.indices
      adjPoints = adjacentPoints(y, x)
      if adjPoints.map(map(_)(_)).min > map(y)(x)
    yield y -> x

  //@tailrec
  def basin(adjPoints: Set[(Int, Int)]): Set[(Int, Int)] =
    adjPoints.foldLeft(adjPoints.filterNot(map(_)(_) == 9)) { case (acc, (y, x)) =>
      acc ++ basin(adjacentPoints(y, x).filter(map(_)(_) > map(y)(x)))
    }

  val basins =
    for lowPoint <- lowPoints yield basin(Set(lowPoint))

  println(s"part 1: ${lowPoints.map(map(_)(_)).sum + lowPoints.size}")

  println(s"part 2: ${basins.map(_.size).toList.sorted.takeRight(3).product}")
