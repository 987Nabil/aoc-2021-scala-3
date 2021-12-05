package aoc

case class BingoBoard(r1: Row, r2: Row, r3: Row, r4: Row, r5: Row):
  private def column(c: Int) =
    productIterator.map { case r: Row => r.productElement(c) }.map { case m: MarkableField => m }

  private def columnMarked(c: Int) =
    column(c).forall(_.marked)

  def won: Boolean =
    (0 until productArity).exists(columnMarked)
      || productIterator.exists { case r: Row => r.marked }

  def mark(draw: Int): BingoBoard =
    copy(r1.mark(draw), r2.mark(draw), r3.mark(draw), r4.mark(draw), r5.mark(draw))

  def score(lastDraw: Int): Int =
    lastDraw * productIterator
      .flatMap { case r: Row => r.productIterator.map { case m: MarkableField => m } }
      .filterNot(_.marked)
      .map(_.value)
      .sum

object BingoBoard:
  private val RowRegEx = """([0-9]*)\s*([0-9]*)\s*([0-9]*)\s*([0-9]*)\s*([0-9]*)""".r

  def fromRowStrings(rows: Seq[String]): BingoBoard =
    rows.map { case RowRegEx(one, two, three, four, five) =>
      Row(one.toInt, two.toInt, three.toInt, four.toInt, five.toInt)
    } match
      case Seq(one, two, three, four, five) => BingoBoard(one, two, three, four, five)

case class Row(
    one: MarkableField,
    two: MarkableField,
    three: MarkableField,
    four: MarkableField,
    five: MarkableField
):
  def marked: Boolean =
    productIterator.forall { case markableField: MarkableField => markableField.marked }

  def mark(draw: Int): Row =
    copy(one.mark(draw), two.mark(draw), three.mark(draw), four.mark(draw), five.mark(draw))

object Row:
  def apply(one: Int, two: Int, three: Int, four: Int, five: Int): Row =
    Row(
      MarkableField(one),
      MarkableField(two),
      MarkableField(three),
      MarkableField(four),
      MarkableField(five)
    )

case class MarkableField(value: Int, marked: Boolean = false):
  def mark(draw: Int): MarkableField = if draw == value then copy(marked = true) else this

@main
def day4(): Unit =
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
