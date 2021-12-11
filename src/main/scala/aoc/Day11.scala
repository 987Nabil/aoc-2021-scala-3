package aoc

import scala.annotation.tailrec
import scala.collection.MapView

@main
def day11(): Unit = day[11] {

  val map  = input.map(_.grouped(1).toSeq.map(_.toInt))
  val indexToValue = {
    for
      x <- map.head.indices
      y <- map.indices
    yield (x, y) -> map.point(x, y)
  }.toMap

  def adjacent(x: Int, y: Int)                        = map.getPoint(x, y).map(_ => (x, y))
  def adjacentPoints(x: Int, y: Int): Set[(Int, Int)] =
    Set((0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, 1), (1, -1)).flatMap((xs, ys) =>
      adjacent(x + xs, y + ys)
    )

  @tailrec
  def flashes(
      map: Map[(Int, Int), Int],
      step: Int,
      flashedCount: Long
  ): (Long, Int) =
    val (newMap, flashed) = recur(map.view.mapValues(_ + 1).toMap, Set.empty)
    if step != 0 && newMap.values.toSet != Set(0) then
      flashes(
        newMap.map { case (k, v) => if flashed.contains(k) then (k, 0) else (k, v) },
        step - 1,
        flashedCount + flashed.size
      )
    else
      (flashedCount, step)

  @tailrec
  def recur(
      map: Map[(Int, Int), Int],
      alreadyFlashed: Set[(Int, Int)]
  ): (Map[(Int, Int), Int], Set[(Int, Int)]) =
    val justFlashed = map.toList.collect { case (k, v) if v > 9 => k }
    val adjPoints   = justFlashed.flatMap(adjacentPoints)
    def newMap =
      map.map { case (k, v) =>
        if (alreadyFlashed ++ justFlashed).contains(k) then (k, 0)
        else if adjPoints.contains(k) then (k, v + adjPoints.count( _ == k))
        else (k, v)
      }

    if (justFlashed.toSet -- alreadyFlashed).isEmpty then (map, alreadyFlashed)
    else
      recur(
        newMap,
        alreadyFlashed ++ justFlashed
      )

  part[1](flashes(indexToValue, 100, 0)._1)
  part[2](1001 - flashes(indexToValue, 1000, 0)._2)
}
