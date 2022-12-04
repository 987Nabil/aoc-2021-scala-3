package aoc

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.constValue
import scala.compiletime.ops.int.-
import scala.concurrent.duration.Duration

private type IntsUntil[N <: Int] <: Int =
  N match {
    case 1 => N
    case _ => N | IntsUntil[N - 1]
  }

type Days = IntsUntil[25]

type Parts = 1 | 2

type Result = Int | Long

inline def timed[T](fn: => T): (T, Duration) =
  val start  = System.currentTimeMillis()
  val result = fn
  (result, Duration(System.currentTimeMillis() - start, TimeUnit.MILLISECONDS))

class Day(day: Int):
  private val parts      = new ArrayBuffer[Part]
  def add(r: Part): Unit = parts += r
  def execute(): Unit    =
    println(formatTable(Seq(Seq("Part", "Duration", "Result")) ++ parts.map(_.process)))

  override def toString: String = parts.map(_.fn.apply()).toString()

class Part(val fn: () => Result, part: Parts):
  def process: Seq[Any] = timed(fn()).pipe((res, duration) => Seq(part, duration, res))

class Input(day: Int):
  val in: Seq[String] = readResourceLines(s"day$day.txt")

inline def input(using Input) = summon[Input].in

inline def day[D <: Days](block: (Day, Input) ?=> Unit): Unit =
  given d: Day   = new Day(constValue[D])
  given i: Input = new Input(constValue[D])
  block
  d.execute()
  ()

inline def part[P <: Parts](fn: => Result)(using d: Day) =
  d.add(Part(() => fn, constValue[P]))

def formatTable(table: Seq[Seq[Any]]): String =
  if table.isEmpty then ""
  else
    // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
    val colWidths =
      table.transpose.map(_.map(cell => if (cell == null) 0 else cell.toString.length).max + 2)
    // Format each row
    val rows      = table
      .zip(Iterable.fill(table.size)(Console.CYAN + Console.BOLD))
      .map((cell, format) =>
        cell.zip(colWidths)
          .map((item, size) => format + (" %-" + (size - 1) + "s").format(item) + Console.RESET)
          .mkString("│".blue, "│".blue, "│".blue)
      )
    // Formatted separator row, used to separate the header and draw table borders
    val separator = colWidths.map("─".blue * _).mkString("┼".blue, "┼".blue, "┼".blue)
    // Put the table together and return
    (separator +: rows.head +: separator +: rows.tail :+ separator).mkString("\n")

extension (s: String)
  def blue = Console.BLUE + s + Console.RESET
  def red = Console.RED + s + Console.RESET
  def green = Console.GREEN + s + Console.RESET
  def magenta = Console.MAGENTA + s + Console.RESET
  def bold = Console.BOLD + s + Console.RESET
