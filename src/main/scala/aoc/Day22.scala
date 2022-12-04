package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

extension (self: Range)
  def overlaps(that: Range) =
    self.contains(that.start) || that.contains(self.start)

  def containsRange(that: Range): Boolean =
    self.start <= that.start && self.last >= that.last

  def merge(that: Range): Range =
    self.head.min(that.head) to self.last.max(that.last)

  def without(that: Range) =
    if that.contains(self.head) then that.last + 1 to self.last
    else self.start to that.start - 1

  def and(that: Range) =
    if that.contains(self.head) then self.start to that.end
    else that.start to self.end

  def startsBefore(that: Range) =
    self.start < that.start

  def endsAfter(that: Range) =
    self.end > that.end

case class Instruction(tpe: InstructionType, xRange: Range, yRange: Range, zRange: Range):
  def isOn: Boolean = tpe == InstructionType.On

  def overlaps(that: Instruction): Boolean =
    tpe == that.tpe
      && xRange.overlaps(that.xRange)
      && yRange.overlaps(that.yRange)
      && zRange.overlaps(that.zRange)

  def contains(that: Instruction): Boolean =
    tpe == that.tpe
      && xRange.containsRange(that.xRange)
      && yRange.containsRange(that.yRange)
      && zRange.containsRange(that.zRange)

//  def merge(that: Instruction): Seq[Instruction] =
//    if this.contains(that) then List(this)
//    else if that.contains(this) then List(that)
//    else if this.overlaps(that) then
//      that.xRange

  infix def ++(that: Instruction): Instruction =
    Instruction(
      that.tpe,
      that.xRange.merge(xRange),
      that.yRange.merge(yRange),
      that.zRange.merge(zRange),
    )

enum InstructionType:
  case On, Off

@main
def day22(): Unit = day[22] {
  val instructions = input.map { case s"$tpe x=$xs..$xe,y=$ys..$ye,z=$zs..$ze" =>
    Instruction(
      InstructionType.valueOf(tpe.capitalize),
      xs.toInt to xe.toInt,
      ys.toInt to ye.toInt,
      zs.toInt to ze.toInt
    )
  }

  @tailrec
  def reduceInstructions(
      instructions: Seq[Instruction],
      reduced: Seq[Instruction]
    ): Seq[Instruction] =
    if instructions.size <= 1 then reduced ++ instructions
    else
      val Seq(head, next) = instructions.take(2)
      println(s"head = ${head}")
      println(s"next = ${next}")
      if head.overlaps(next) then
        reduceInstructions((head ++ next).tap(println) +: instructions.drop(2), reduced)
      else reduceInstructions(instructions.drop(1), reduced :+ head)

  def cubing(state: Map[(Int, Int, Int), Boolean], instructions: Instruction) =
    state ++ {
      for
        x <- instructions.xRange
        y <- instructions.yRange
        z <- instructions.zRange
      yield (x, y, z) -> instructions.isOn
    }.toMap

  val x1 = 0 to 4
  val y1 = 2 to 8
  val x2 = -2 to 2
  val y2 = 4 to 11

  @tailrec
  def xxxx(c1: Instruction, c2: Instruction): List[(Range, Range, Range)] =
    val one @ (x1, y1, z1) = (c1.xRange, c1.yRange, c1.zRange)
    val (x2, y2, z2)       = (c2.xRange, c2.yRange, c2.zRange)
    if List(x2.containsRange(x1), y2.containsRange(y1), z2.containsRange(z1))
        .count(identity) == 2
    then xxxx(c2, c1)
    else if x1.containsRange(x2) && y1.containsRange(y2) then List(one, (x2, y2, z2.without(z1)))
    else if x1.containsRange(x2) && z1.containsRange(z2) then List(one, (x2, y2.without(y1), z2))
    else if y1.containsRange(y2) && z1.containsRange(z2) then List(one, (x2.without(x1), y2, z2))
    else if x2.endsAfter(x1) || x2.startsBefore(x1) then
      List(
        one,
        (x2.without(x1), y2, z2),
        (x1.and(x2), y2.without(y1), z2),
        (x1.and(x2), y2.without(y1), z2.without(z1)),

      )
    else throw new Exception("NO!")

  y2.span(y1.contains).tap(println)
  x2.span(_ != x1.start).tap(println)
  val yReduced = y2.without(y1).tap(println)
  val cube1    = (x2, yReduced).tap(println)
  val cube2    = (x2.without(x1), y2.start to yReduced.start - 1).tap(println)
//  xxxx(instructions.head, instructions.tail.head).tap(println).tap{
//    _.foreach{(xR,yR,zR) =>
//      for
//        x <- xR
//        y <- yR
//        z <- zR
//      do println(s"$x,$y,$z")
//    }
//  }
//    .foldLeft(0)((i, next) =>
//      i + (next._1.end - next._1.start + 1)
//        * (next._2.end - next._2.start + 1)
//        * (next._3.end - next._3.start + 1)
//    )
//    .tap(println)

  part[1](
    instructions // reduceInstructions(instructions, Nil)
      .filter(i => i.xRange.max.pipe(max => max <= 50 && max >= -50))
      .foldLeft(Map.empty[(Int, Int, Int), Boolean])(cubing)
      .values
      .count(_ == true)
  )
  part[2](
    instructions
      .foldLeft(0)((i, next) =>
        i + (next.xRange.end - next.xRange.start + 1) * (next.yRange.end - next.yRange.start + 1) * (next.zRange.end - next.zRange.start + 1)
      )
  )

}
