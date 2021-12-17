package aoc

import aoc.*

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day17(): Unit = day[17] {

  val (xRange, yRange) = input.head
    .pipe { case s"target area: x=$sx..$ex, y=$sy..$ey" =>
      (sx.toInt to ex.toInt, sy.toInt to ey.toInt)
    }

  type Velocity = (Int, Int)

  def newVelocity(velocity: (Int, Int)) =
    (velocity.x + velocity.x.compareTo(0) * -1, velocity.y - 1)

  def newPosition(velocity: (Int, Int), pos: (Int, Int)) =
    (pos.x + velocity.x, pos.y + velocity.y)

  def step(velocity: Velocity, pos: Coord): List[(Velocity, Coord)] =
    if pos.x > xRange.max || pos.y < yRange.min then Nil
    else (velocity, pos) :: step(newVelocity(velocity), newPosition(velocity, pos))

  def probePaths = for
    startVX <- 0 to xRange.max
    startVY <- yRange.min to yRange.min * -1
  yield step((startVX, startVY), (0, 0))

  def landsInTargetArea(path: List[(Velocity, Coord)]) =
    path.exists((_, pos) => xRange.contains(pos.x) && yRange.contains(pos.y))

  def allValidPaths = probePaths.filter(landsInTargetArea)

  def maxYVelocityToHit = allValidPaths.map(_.map((_, pos) => pos.y).max).max

  part[1](maxYVelocityToHit)
  part[2](allValidPaths.size)

}
