package aoc

import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Day4v2:
  opaque type BingoBoard = (Row, Row, Row, Row, Row)

  object BingoBoard:
    import MarkableField.*
    import Row.*
    extension (board: BingoBoard)
      private def column(c: Int): List[MarkableField] =
        board.toList.map(_.toList(c))

      private def columnMarked(c: Int) =
        board.column(c).forall(_.fieldMarked)

      def won: Boolean =
        (0 until board.productArity).exists(columnMarked)
          || board.toList.exists { case r: Row => r.marked }

      def mark(draw: Int): BingoBoard =
        board.map[[R] =>> Row]([R] => (_: R) match { case r: Row => r.markRow(draw) })

      def score(lastDraw: Int): Int =
        lastDraw * board.toList
          .flatMap { case r: Row => r.toList.map { case m: MarkableField => m } }
          .filterNot(_.fieldMarked)
          .map(_.value)
          .sum

  end BingoBoard

  opaque type MarkableField = (Int, Boolean)

  object MarkableField:
    private val RowRegEx = """([0-9]*)\s*([0-9]*)\s*([0-9]*)\s*([0-9]*)\s*([0-9]*)""".r

    def apply(value: String): MarkableField = (value.toInt, false)

    def fromRowStrings(rows: Seq[String]): BingoBoard =
      rows match
        case Seq(one, two, three, four, five) =>
          (asRow(one), asRow(two), asRow(three), asRow(four), asRow(five))

    private def asRow(s: String): Row =
      s match
        case RowRegEx(one, two, three, four, five) =>
          (apply(one), apply(two), apply(three), apply(four), apply(five))

    extension (mf: MarkableField)
      def markField(draw: Int): MarkableField =
        val (value, marked) = mf
        if draw == value then (value, true) else mf

      def fieldMarked: Boolean = mf._2
      def value: Int           = mf._1
  end MarkableField

  opaque type Row <: (MarkableField, MarkableField, MarkableField, MarkableField, MarkableField) =
    (MarkableField, MarkableField, MarkableField, MarkableField, MarkableField)

  object Row:
    import MarkableField.*
    extension (r: Row)
      def marked: Boolean =
        r.toList.forall { case mf: MarkableField => mf.fieldMarked }

      def markRow(draw: Int): Row =
        r.map[[M] =>> MarkableField](
          [M] => (_: M) match { case m: MarkableField => m.markField(draw) }
        )

@main
def day4_v2(): Unit =
  val input  = readResourceLines("day4.txt")
  val draws  = input.head.split(',').map(_.toInt)
  val boards = input.tail.filterNot(_.isBlank).grouped(5).map(BingoBoard.fromRowStrings).toSeq

  val (_, Some(winningBoard), lastDraw) =
    draws.foldLeft((boards, Option.empty[BingoBoard], 0)) {
      case (winner @ (_, Some(_), _), _)                => winner
      case ((boards, winningBoard, lastDraw), nextDraw) =>
        val newBoards   = boards.map(_.mark(nextDraw))
        val mayBeWinner = newBoards.find(_.won)
        (newBoards, mayBeWinner, nextDraw)
    }

  val (_, Some(lastWinningBoard, lastLastDraw)) =
    draws.foldLeft((boards, Option.empty[(BingoBoard, Int)])) {
      case ((boards, winningBoard), nextDraw) =>
        val newBoards   = boards.map(_.mark(nextDraw))
        val mayBeWinner = newBoards.find(_.won).map((_, nextDraw))
        (newBoards.filterNot(_.won), mayBeWinner.orElse(winningBoard))
    }

  println(s"part 1: ${winningBoard.score(lastDraw)}")
  println(s"part 2: ${lastWinningBoard.score(lastLastDraw)}")
