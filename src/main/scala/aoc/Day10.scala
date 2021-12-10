package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack

@main
def day10(): Unit = day[10] {
  val syntax = input

  val open  = Set('(', '<', '{', '[')
  val close = Set(')', '>', '}', ']')

  val score =
    Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

  val score2 =
    Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

  @tailrec
  def firstInvalidBrace(stack: Stack[Char], line: String): (Option[Char], Option[Stack[Char]]) =
    line.toList match
      case next :: _ =>
        if open(next) then firstInvalidBrace(stack.push(next), line.tail)
        else if (stack.head - next).abs <= 2 then firstInvalidBrace(stack.drop(1), line.tail)
        else (Some(next), None)
      case Nil       => (None, Some(stack))

  val invalidBracesScore = syntax
    .flatMap { l =>
      firstInvalidBrace(Stack.empty[Char].pushAll(l.takeWhile(open)), l.dropWhile(open))._1
    }
    .map(score)
    .sum

  val bla = syntax
    .flatMap { l =>
      firstInvalidBrace(Stack.empty[Char].pushAll(l.takeWhile(open)), l.dropWhile(open))._2
    }
    .map {
      _.toList
        .flatMap(open => close.find(c => (open - c).abs <= 2))
        .map(score2)
        .foldLeft(0L)((score, next) => score * 5 + next)
    }
    .sorted

  println(s"bla = ${bla}")
  part[1](invalidBracesScore)
  part[2](bla(bla.size / 2))
}
