package aoc

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.constValue
import scala.compiletime.ops.int.-
import scala.concurrent.duration.Duration

private type DayHelper[N <: Int] =
  N match {
    case 1 => N
    case _ => N | DayHelper[N - 1]
  }

type Days = DayHelper[25]

type Parts = 1 | 2

type Result = Int | Long

def timed[T](fn: => T): (T, Duration) =
  val start  = System.currentTimeMillis()
  val result = fn
  (result, Duration(System.currentTimeMillis() - start, TimeUnit.MILLISECONDS))

class Day:
  private val parts      = new ArrayBuffer[Part]
  def add(r: Part): Unit = parts += r
  def execute(): Unit    =
    parts.foreach(_.execute())

  override def toString: String = parts.map(_.fn.apply()).toString()

class Part(val fn: () => Result, part: Parts):
  def execute(): Unit =
    timed(fn()).pipe((res, duration) =>
      println(s"Part $part took $duration to compute the result $res")
    )

class Input(day: Int):
  val in: Seq[String] = readResourceLines(s"day$day.txt")

inline def input(using Input) = summon[Input].in

inline def day[D <: Days](block: (Day, Input) ?=> Unit): Unit =
  given d: Day   = new Day
  given i: Input = new Input(constValue[D])
  block
  d.execute()
  ()

inline def part[P <: Parts](fn: => Result)(using d: Day) =
  d.add(Part(() => fn, constValue[P]))
