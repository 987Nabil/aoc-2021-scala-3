package aoc

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

@main
def day1(): Unit =
  val depthSeq = readResourceLines("day1.txt").map(_.toInt)

  val countOne = countIncreasingDepth(depthSeq)
  val countTwo = countIncreasingDepth(depthSeq.sliding(3).toSeq.map(_.sum))

  println(s"part one: $countOne")
  println(s"part two: $countTwo")

def countIncreasingDepth(depthSeq: Seq[Int]): Int =
  depthSeq.sliding(2).count { case one :: two :: Nil => one < two }
