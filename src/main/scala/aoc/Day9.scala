package aoc

import aoc.{getPoint, point}

import scala.annotation.tailrec

@main
def day9(): Unit =
  val map = readResourceLines("day9.txt").map(_.grouped(1).toSeq.map(_.toInt))

  def adjacent(x: Int, y: Int) = map.getPoint(x, y).map(_ => (x, y))

  def adjacentPoints(x: Int, y: Int): Set[(Int, Int)] =
    Set(adjacent(x, y - 1), adjacent(x, y + 1), adjacent(x - 1, y), adjacent(x + 1, y)).flatten

  val lowPoints =
    for
      x        <- map.head.indices
      y        <- map.indices
      adjPoints = adjacentPoints(x, y)
      if adjPoints.map(map.point).min > map.point(x, y)
    yield x -> y

  @tailrec
  def basin(points: Set[(Int, Int)], acc: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] =
    if points.isEmpty then acc
    else
      points
        .flatMap((x, y) => adjacentPoints(x, y).filter(map.point(_, _) > map.point(x, y)))
        .pipe(newPoints => basin(newPoints, (acc ++ newPoints).filter(map.point(_, _) != 9)))

  val basins = for lowPoint <- lowPoints yield basin(Set(lowPoint), Set(lowPoint))

  println(s"part 1: ${lowPoints.map(map.point).sum + lowPoints.size}")
  println(s"part 2: ${basins.map(_.size).toList.sorted.takeRight(3).product}")
