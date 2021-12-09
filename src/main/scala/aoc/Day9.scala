package aoc

import aoc.{getPoint, point}

import scala.annotation.tailrec
import scala.util.chaining.*

@main
def day9(): Unit =
  val map = readResourceLines("day9.txt").map(_.grouped(1).toSeq.map(_.toInt))

  def maybeAdjacent(x: Int, y: Int) =
    map.getPoint(x, y).map(_ => (x, y))

  def adjacentPoints(x: Int, y: Int): Set[(Int, Int)] =
    Set(
      maybeAdjacent(x, y - 1),
      maybeAdjacent(x, y + 1),
      maybeAdjacent(x - 1, y),
      maybeAdjacent(x + 1, y)
    ).flatten

  val lowPoints =
    for
      x        <- map.head.indices
      y        <- map.indices
      adjPoints = adjacentPoints(x, y)
      if adjPoints.map(map.point).min > map.point(x, y)
    yield x -> y

  // @tailrec
  def basin(adjPoints: Set[(Int, Int)]): Set[(Int, Int)] =
    adjPoints.foldLeft(adjPoints.filterNot(map.point(_, _) == 9)) { case (acc, (x, y)) =>
      acc ++ basin(adjacentPoints(x, y).filter(map.point(_, _) > map.point(x, y)))
    }

  val basins = for lowPoint <- lowPoints yield basin(Set(lowPoint))

  println(s"part 1: ${lowPoints.map(map.point).sum + lowPoints.size}")
  println(s"part 2: ${basins.map(_.size).toList.sorted.takeRight(3).product}")
