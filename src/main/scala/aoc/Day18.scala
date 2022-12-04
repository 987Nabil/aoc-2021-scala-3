package aoc

import aoc.*
import aoc.SnailNumber.SnailNode

import java.util.UUID
import scala.annotation.tailrec

enum SnailNumber:
  case SnailNode(left: SnailNumber, right: SnailNumber)
  case Num(n: Int)

  val id: UUID = UUID.randomUUID()

  override def equals(that: Any): Boolean =
    that match { case that: SnailNumber => this.id == that.id }

  def depth: Int =
    this match
      case SnailNode(l, r) => 1 + l.depth.max(r.depth)
      case _               => 0

  def asList: List[SnailNumber] =
    this match
      case SnailNode(left, right) => List(this) ++ left.asList ++ right.asList
      case _                      => List(this)

  def replace(remove: SnailNumber, replace: SnailNumber): SnailNumber =
    this match
      case self if self.id == remove.id                              => replace
      case SnailNode(l, r) if l.id == remove.id                      => SnailNode(replace, r)
      case SnailNode(l, r) if r.id == remove.id                      => SnailNode(l, replace)
      case SnailNode(l, r) if l.asList.map(_.id).contains(remove.id) =>
        SnailNode(l.replace(remove, replace), r)
      case SnailNode(l, r) if r.asList.map(_.id).contains(remove.id) =>
        SnailNode(l, r.replace(remove, replace))
      case other                                                     => other

  def isNum: Boolean =
    this match
      case Num(_) => true
      case _      => false

  infix def +(that: SnailNumber): SnailNumber =
    (this, that) match
      case (Num(thisN), Num(thatN)) => Num(thisN + thatN)

  infix def ++(that: SnailNumber): SnailNumber =
    (this, that) match
      case (sn1: SnailNode, sn2: SnailNode) => SnailNode(sn1, sn2)

  override def toString: String =
    this match
      case Num(n)          => n.toString
      case SnailNode(l, r) => s"[${l.toString},${r.toString}]"

  def magnitude: Long =
    this match
      case SnailNode(l, r) => 3 * l.magnitude + 2 * r.magnitude
      case Num(n)          => n

@main
def day18(): Unit = day[18] {

  def parseSnailNumber(s: String): SnailNumber =
    s.head match
      case '[' =>
        s.tail.splitAt(commaIndex(s.tail)).pipe { (left, right) =>
          SnailNumber.SnailNode(parseSnailNumber(left), parseSnailNumber(right))
        }
      case ',' => parseSnailNumber(s.tail)
      case _   => SnailNumber.Num(s.takeWhile(_.isDigit).toInt)

  @tailrec
  def commaIndex(s: String, index: Int = 0, depth: Int = 0): Int =
    s.head match
      case ',' if depth == 0 => index
      case '['               => commaIndex(s.tail, index + 1, depth + 1)
      case ']'               => commaIndex(s.tail, index + 1, depth - 1)
      case _                 => commaIndex(s.tail, index + 1, depth)

  def splitableSnailNumber(snailNumber: SnailNumber) = {
    snailNumber.asList.collectFirst { case sn@SnailNumber.Num(n) if n >= 10 => sn }
  }

  def explodeableSnailNumber(snailNumber: SnailNumber) = {
    snailNumber.asList
      .find(_.depth == 5)
      .map(
        _.asList.pipe(
          l =>
            l.reduce { (x, y) => if x.depth == 1 then x else if x.depth - 1 == y.depth then y else x }
          )
        )
  }

  @tailrec
  def reduce(snailNumber: SnailNumber): SnailNumber =
    val maybeExplode = explodeableSnailNumber(snailNumber)
    val maybeSplit   = splitableSnailNumber(snailNumber)
    if maybeExplode.isDefined then
      maybeExplode
        .map { case x @ SnailNumber.SnailNode(ln @ SnailNumber.Num(_), rn @ SnailNumber.Num(_)) =>
          snailNumber.asList
            .splitAt(snailNumber.asList.indexOf(x))
            .pipe { (left: List[SnailNumber], right: List[SnailNumber]) =>
              (left.findLast(_.isNum), right.drop(3).find(_.isNum)).pipe { case (l, r) =>
                val leftReplacement  = l.map { l => l -> (l + ln) }
                val rightReplacement = r.map { r => r -> (r + rn) }

                val withoutX      = snailNumber.replace(x, SnailNumber.Num(0))
                val replacedLeft  = leftReplacement.fold(withoutX)(withoutX.replace)
                val replacedRight = rightReplacement.fold(replacedLeft)(replacedLeft.replace)
                replacedRight
              }
            }

        }
        .get
        .pipe(reduce)
    else if maybeSplit.isDefined then
      maybeSplit
        .map {
          case s @ SnailNumber.Num(n) if n % 2 == 0 =>
            s -> SnailNumber.SnailNode(SnailNumber.Num(n / 2), SnailNumber.Num(n / 2))
          case s @ SnailNumber.Num(n) =>
            s -> SnailNumber.SnailNode(SnailNumber.Num(n / 2), SnailNumber.Num(n / 2 + 1))
        }
        .map(snailNumber.replace)
        .get
        .pipe(reduce)
    else snailNumber

  val snailNumbers: Seq[SnailNumber] = input.map(parseSnailNumber)

  def part1 = snailNumbers.map(reduce).reduce((sn1, sn2) => reduce(sn1 ++ sn2)).magnitude

  def part2 = snailNumbers
    .combinations(2)
    .flatMap { x =>
      x.map(reduce).reduce((sn1, sn2) => reduce(sn1 ++ sn2)).magnitude ::
        x.reverse.map(reduce).reduce((sn1, sn2) => reduce(sn1 ++ sn2)).magnitude :: Nil
    }
    .max

  part[1](part1)
  part[2](part2)

}
