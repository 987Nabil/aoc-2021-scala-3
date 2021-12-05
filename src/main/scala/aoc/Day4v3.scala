package aoc

import scala.collection.immutable.{AbstractSeq, LinearSeq}

@main
def day4_v3(): Unit =
  val input  = readResourceLines("day4.txt")
  val draws  = input.head.split(',').map(_.toInt).toSeq.inits.toSeq.reverse
  val boards = input.tail.filterNot(_.isBlank).grouped(5).map(fromRowStrings).toSeq

  val (Some(winningBoard), lastDraws) =
    draws.foldLeft((Option.empty[Seq[Seq[Int]]], Seq.empty[Int])) {
      case (winner @ (Some(_), _), _)           => winner
      case ((winningBoard, lastDraw), nextDraw) =>
        val mayBeWinner = boards.find(won(nextDraw.toSet))
        (mayBeWinner, nextDraw)
    }

  val (_, Some(lastWinningBoard, lastLastDraw)) =
    draws.foldLeft((boards, Option.empty[(Seq[Seq[Int]], Seq[Int])])) {
      case ((boards, winningBoard), nextDraw) =>
        val mayBeWinner = boards.find(won(nextDraw.toSet)).map((_, nextDraw))
        (boards.filterNot(won(nextDraw.toSet)), mayBeWinner.orElse(winningBoard))
    }

  println(s"part 1: ${score(winningBoard, lastDraws)}")
  println(s"part 2: ${score(lastWinningBoard, lastLastDraw)}")

def fromRowStrings(rows: Seq[String]): Seq[Seq[Int]] =
  rows.map(_.split("""\s+""").toSeq.map(_.toInt))

def won(draw: Set[Int])(board: Seq[Seq[Int]]): Boolean =
  containsSubset(board, draw) || containsSubset(board.transpose, draw)

private def containsSubset(board: Seq[Seq[Int]], draw: Set[Int]) =
  board.exists(_.toSet.subsetOf(draw))

def score(board: Seq[Seq[Int]], lastDraws: Seq[Int]): Int =
  board.flatten.filterNot(lastDraws.toSet).sum * lastDraws.last
