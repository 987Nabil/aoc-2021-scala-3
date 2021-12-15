package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day15(): Unit = day[15] {

  val map: CoordMap = input
    .map(_.grouped(1).toSeq.map(_.toInt))
    .pipe { map =>
      for
        x <- map.head.indices
        y <- map.indices
      yield (x, y) -> map.point(x, y)
    }
    .toMap

  val xMax = map.keySet.map(_._1).max
  val yMax = map.keySet.map(_._2).max

  def nextValue(v: Int, i: Int) =
    if (v + i) % 9 == 0 then 9 else (v + i) % 9

  val extendedMap = (1 to 4).flatMap { i =>
    (1 to 4).flatMap { j =>
      map.flatMap { case old @ ((x, y), value) =>
        val newX = x + (xMax + 1) * i
        val newY = y + (yMax + 1) * j
        List(
          (newX, y)    -> nextValue(value, i),
          (x, newY)    -> nextValue(value, j),
          (newX, newY) -> nextValue(value, i + j)
        )
      }
    }
  }.toMap ++ map

  val xMax2 = extendedMap.keySet.map(_._1).max
  val yMax2 = extendedMap.keySet.map(_._2).max

  def dijkstra(start: Coord, map: CoordMap): CoordMap =
    val distances = mutable.Map((map.map((k, _) => k -> Int.MaxValue) + ((0, 0) -> 0)).toSeq*)
    val unvisited = mutable.Set(0 -> 0)
    while unvisited.nonEmpty do
      val current = unvisited.minBy(distances)
      unvisited -= current
      for
        neighbour <- map.nonDiagonalNeighbours(current)
        newDist    = distances(current) + map(neighbour)
      do
        if newDist < distances(neighbour) then
          distances += neighbour -> newDist
          unvisited += neighbour
    distances.toMap

  lazy val pathValues = dijkstra((0, 0), map)

  part[1](pathValues(xMax, yMax))
  part[2](dijkstra((0, 0), extendedMap)(xMax2, yMax2))

}
