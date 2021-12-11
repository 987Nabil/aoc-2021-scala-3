package aoc

import scala.annotation.tailrec
import scala.collection.MapView

@main
def day11(): Unit = day[11] {

  val map = input.map(_.grouped(1).toSeq.map(_.toInt)).pipe { map =>
    for
      x <- map.head.indices
      y <- map.indices
    yield (x, y) -> map.point(x, y)
  }.toMap

  part[1](flashes(map, 100, 0)._1)
  part[2](1000 - flashes(map, 1000, 0)._2)
}

@tailrec
def flashes(map: CoordMap, step: Int, flashed: Long): (Long, Int) =
  val (newMap, justFlashed) = processStep(map.view.mapValues(_ + 1).toMap, Set.empty)
  if step != 0 && newMap.values.sum != 0 then flashes(newMap, step - 1, flashed + justFlashed)
  else (flashed, step - 1)

@tailrec
def processStep(map: CoordMap, alreadyFlashed: Set[Coord]): (CoordMap, Int) =
  if noNewFlashes(alreadyFlashed, justFlashed(map)) then (map, alreadyFlashed.size)
  else processStep(updateMap(map, alreadyFlashed), alreadyFlashed ++ justFlashed(map))

def justFlashed(map: CoordMap): List[Coord] =
  map.toList.collect { case (k, v) if v > 9 => k }

def flashedNeighbours(map: CoordMap): List[Coord] =
  justFlashed(map).flatMap(map.neighbours)

def noEnergy(map: CoordMap, alreadyFlashed: Set[Coord]): CoordMap =
  (alreadyFlashed ++ justFlashed(map)).map(_ -> 0).toMap

def updateMap(map: CoordMap, alreadyFlashed: Set[Coord]): CoordMap =
  map ++ energizedNeighbours(map) ++ noEnergy(map, alreadyFlashed)

def energizedNeighbours(map: CoordMap): CoordMap =
  flashedNeighbours(map).map(k => k -> (map(k) + flashedNeighbours(map).count(_ == k))).toMap

def noNewFlashes(alreadyFlashed: Set[Coord], justFlashed: List[Coord]): Boolean =
  justFlashed.toSet.subsetOf(alreadyFlashed)
