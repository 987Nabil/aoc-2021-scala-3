package aoc

import scala.annotation.tailrec
import scala.collection.MapView

@main
def day12(): Unit = day[12] {

  val connections =
    input.flatMap { case s"$x-$y" => List(x -> y, y -> x) }
      .filterNot(_._2 == "start").groupMap(_._1)(_._2) - "end"

  @tailrec
  def paths(visited: List[List[String]]): List[List[String]] =
    val newPaths = visited.flatMap {
      case path if path.last == "end" => List(path)
      case path if path.groupMapReduce(identity)(_ => 1)(_ + _).filterKeys(_.forall(_.isLower)).values.exists(_ > 1) => List()
      case path                       => connections(path.last).map(x => path :+ x)
    }
    if newPaths == visited then visited else paths(newPaths)

  def filterPaths2(path: List[String]) =
    val smallCavesCounts = path.groupMapReduce(identity)(_ => 1)(_ + _).filterKeys(_.forall(_.isLower)).values
    smallCavesCounts.count(_ == 2) > 1 || smallCavesCounts.exists(_ > 2)

  @tailrec
  def paths2(visited: List[List[String]]): List[List[String]] =
    val newPaths = visited.flatMap {
      case path if path.last == "end" => List(path)
      case path if filterPaths2(path) => List()
      case path                       => connections(path.last).map(x => path :+ x)
    }
    if newPaths == visited then visited else paths2(newPaths)

  part[1](paths(List(List("start"))).size)
  part[2](paths2(List(List("start"))).size)
}
