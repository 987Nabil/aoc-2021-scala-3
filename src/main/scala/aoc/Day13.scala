package aoc

import aoc.Day12Types.*

import scala.annotation.tailrec
import scala.collection.MapView

@main
def day13(): Unit = day[13] {
  val dots  = input.takeWhile(!_.isBlank).map { case s"$x,$y" => (x.toInt, y.toInt) }
  val folds = input.dropWhile(!_.isBlank).tail.map { case s"fold along $c=$n" => (c, n.toInt) }

  val xMax = dots.maxBy(_._1)._1
  val yMax = dots.maxBy(_._2)._2

  def foldAlong(dots: Seq[(Int, Int)])(c: String, n: Int): Seq[(Int, Int)] =
    c match
      case "x" =>
        for
          xCoord     <- n to xMax
          foldedDots <-
            dots.collect {
              case (x, y) if x == n - (xMax - xCoord) => (x, y)
              case (x, y) if x == n + (xMax - xCoord) => (n - (xMax - xCoord), y)
            }
        yield foldedDots
      case "y" =>
        for
          yCoord     <- n to yMax
          foldedDots <-
            dots.collect {
              case (x, y) if y == n - (yMax - yCoord) => (x, y)
              case (x, y) if y == n + (yMax - yCoord) => (x, n - (yMax - yCoord))
            }
        yield foldedDots

  val folded = folds.foldLeft(dots)(foldAlong(_).tupled(_))

  part[1](foldAlong(dots).tupled(folds.head).toSet.size)

  {
    val xMax = folded.maxBy(_._1)._1
    val yMax = folded.maxBy(_._2)._2

    val foldedMap = folded.map(_ -> "#").toMap.withDefaultValue(" ")

    val replaced =
      for
        y <- 0 to yMax
        x <- 0 to xMax
      yield foldedMap(x, y)
    .mkString

    println("Part 2:")
    replaced
      .grouped(xMax + 1)
      .map(_.flatMap(x => if x == '#' then x.toString.red.bold else x.toString))
      .foreach(println)
  }

}
