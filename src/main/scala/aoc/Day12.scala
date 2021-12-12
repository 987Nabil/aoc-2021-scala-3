package aoc

import aoc.Day12Types.*

import scala.annotation.tailrec
import scala.collection.MapView

@main
def day12(): Unit = day[12] {

  val connections =
    input
      .flatMap { case s"$x-$y" => List(Cave(x) -> Cave(y), Cave(y) -> Cave(x)) }
      .filterNot(_.connectsToStart)
      .filterNot(_.connectsFromEnd)
      .groupMap(_._1)(_._2)

  def deadEnd(path: Path) =
    path.smallCaves.caveVisits.values.exists(_ > 1)

  def deadEnd2(path: Path) =
    path.smallCaves.caveVisits.values.count(_ == 2) > 1
      || path.smallCaves.caveVisits.values.exists(_ > 2)

  @tailrec
  def paths(visited: List[Path], deadEnd: Path => Boolean): List[Path] =
    val newPaths = visited.flatMap {
      case path if path.endReached => List(path)
      case path                    => connections(path.currentCave).map(path.goTo)
    }
    if newPaths == visited then visited else paths(newPaths.filterNot(deadEnd), deadEnd)

  part[1](paths(List(Path.Start), deadEnd).size)
  part[2](paths(List(Path.Start), deadEnd2).size)
}

object Day12Types:
  opaque type Cave = String
  opaque type Path = List[Cave]

  object Cave:
    def apply(cave: String): Cave = cave
    val End: Cave                 = "end"
    val Start: Cave               = "start"

  object Path:
    val Start: Path = List(Cave.Start)
    val End: Path   = List(Cave.End)

  extension (path: Path)
    def smallCaves: Path           = path.filter(_.forall(_.isLower))
    def caveVisits: Map[Cave, Int] = path.groupMapReduce(identity)(_ => 1)(_ + _)
    def goTo(cave: Cave): Path     = path :+ cave
    def currentCave: Cave          = path.last
    def endReached: Boolean        = path.currentCave == Cave.End

  extension (connection: (Cave, Cave))
    def connectsToStart: Boolean = connection._2 == Cave.Start
    def connectsFromEnd: Boolean = connection._1 == Cave.End
