package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}
import scala.compiletime.ops.int.-

case class State(
    hallway: Hallway,
    r1: Room,
    r2: Room,
    r3: Room,
    r4: Room,
    energyUsed: Int,
  ):
  val rooms: List[Room]          = List(r1, r2, r3, r4)
  val smallestPodToMove: Room    = rooms.minBy(_.pods.find(_.occupied))
  val movablePods: Set[Amphipod] = ???
  val canMoveIn: Set[Amphipod]   =
    rooms.collect { case r if r.pods.forall(_ == r.owner) => r.owner }.toSet
  val canMoveOutOfRoom: Any = rooms.flatMap(_.pods.find(_.occupied))

  //format: off
  override def toString: String =
    s"""
#############
#...........#
###${prettyPrint(r1.pods(0))}#${prettyPrint(r2.pods(0))}#${prettyPrint(r3.pods(0))}#${prettyPrint(r4.pods(0))}###
  #${prettyPrint(r1.pods(1))}#${prettyPrint(r2.pods(1))}#${prettyPrint(r3.pods(1))}#${prettyPrint(r4.pods(1))}#
  #${prettyPrint(r1.pods(2))}#${prettyPrint(r2.pods(2))}#${prettyPrint(r3.pods(2))}#${prettyPrint(r4.pods(2))}#
  #${prettyPrint(r1.pods(3))}#${prettyPrint(r2.pods(3))}#${prettyPrint(r3.pods(3))}#${prettyPrint(r4.pods(3))}#
  #########
    """
  //format: on
def prettyPrint(a: Amphipod | '.') =
  a match {
    case Amphipod.A => Amphipod.A.toString.red
    case Amphipod.B => Amphipod.B.toString.blue
    case Amphipod.C => Amphipod.C.toString.green
    case Amphipod.D => Amphipod.D.toString.magenta
    case '.'        => "."
  }

extension (pod: Amphipod | '.') def occupied: Boolean = pod != '.'

case class Hallway(positions: Seq[Amphipod | '.']):
  val r1: Amphipod | '.' = positions(10)
  val r2: Amphipod | '.' = positions(9)
  val l1: Amphipod | '.' = positions(0)
  val l2: Amphipod | '.' = positions(1)

object Hallway:
  val empty: Hallway = Hallway(List.fill(11)('.'))

case class Room(pods: Seq[Amphipod | '.'], owner: Amphipod, id: Int):
  val notHome:Seq[Amphipod] = pods.collect{ case a:Amphipod if a != owner => a}

enum Amphipod(val energy: Int):
  case A extends Amphipod(1)
  case B extends Amphipod(10)
  case C extends Amphipod(100)
  case D extends Amphipod(1000)

given Ordering[Amphipod | '.'] with
  override def compare(x: Amphipod | '.', y: Amphipod | '.'): Int =
    (x, y) match
      case (x: Amphipod, y: Amphipod) => x.energy.compareTo(y.energy)
      case (x: Amphipod, y: '.')      => -1
      case (x: '.', y: Amphipod)      => 1
      case (x: '.', y: '.')           => 0

@main
def day23(): Unit = day[23] {
  // val RegEx                                      = """\s*#*([A-D])#([A-D])#([A-D])#([A-D]).*""".r
//  type Field = Amphipod | '#' | '.' | ' '
//  def parse(c: Char): Amphipod | '#' | '.' | ' ' =
//    c match
//      case '#'   => '#'
//      case '.'   => '.'
//      case ' '   => ' '
//      case other => Amphipod.valueOf(other.toString)
//
//  val initialState = {
//    for
//      x <- 0 to input.head.length
//      y <- 0 to input.size
//    yield (x, y) -> input.lift(x).flatMap(_.lift(y)).map(parse).fold[Amphipod | '#' | '.' | ' ']('.')(identity)
//  }.toMap

//  def solve(state: State) =
//    val roomToEmpty = state.rooms(2)
//    for
//      pod <- roomToEmpty.notHome
//

//    input.collect { case RegEx(r1, r2, r3, r4) =>
//      List(
//        Amphipod.valueOf(r1),
//        Amphipod.valueOf(r2),
//        Amphipod.valueOf(r3),
//        Amphipod.valueOf(r4),
//      )
//    }
//    .transpose
//    .pipe { case Seq(r1, r2, r3, r4) =>
//      State(
//        Hallway.empty,
//        Room(r1, Amphipod.A),
//        Room(r2, Amphipod.B),
//        Room(r3, Amphipod.C),
//        Room(r4, Amphipod.D),
//        energyUsed = 0,
//      )
//    }
//    .tap(println)

//  def move(s: State) =
//    if (s.movablePods & s.canMoveIn).nonEmpty then move(s.movePodsIn)
//
//  move(initialState)

  part[1](0)
  part[2](0)

}
