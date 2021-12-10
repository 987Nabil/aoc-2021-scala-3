package aoc

import scala.annotation.tailrec

@main
def day10(): Unit = day[10] {
  val open  = Set('(', '<', '{', '[')
  val close = Set(')', '>', '}', ']')

  val errorScore        = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val autoCompleteScore = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  @tailrec
  def invalidOrLeftOpenBraces(stack: String, line: String): Char | String =
    line.toList match
      case next :: _ =>
        if open(next) then invalidOrLeftOpenBraces(stack :+ next, line.tail)
        else if (stack.head - next).abs <= 2 then invalidOrLeftOpenBraces(stack.drop(1), line.tail)
        else next
      case Nil       => stack

  val (errorLines, incompleteLines) =
    input.map(l => invalidOrLeftOpenBraces(l.takeWhile(open), l.dropWhile(open))).pipe { x =>
      (x.collect { case c: Char => c }, x.collect { case s: String => s })
    }

  val invalidBracesScore = errorLines.map(errorScore).sum

  val autoCompleteScores = incompleteLines.map {
    _.flatMap(open => close.find(c => (open - c).abs <= 2))
      .map(autoCompleteScore)
      .foldLeft(0L)((score, next) => score * 5 + next)
  }.sorted

  part[1](invalidBracesScore)
  part[2](autoCompleteScores(autoCompleteScores.size / 2))
}
