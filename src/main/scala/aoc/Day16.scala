package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

enum Package:
  case Literal(version: Long, override val value: Long)
  case Operator(version: Long, packageType: Long, subs: List[Package])

  def versionSum: Long =
    this match
      case l: Package.Literal  => l.version
      case o: Package.Operator => o.version + o.subs.map(_.versionSum).sum

  def value: Long =
    this match
      case l: Package.Literal                     => l.value
      case Package.Operator(_, packageType, subs) =>
        packageType match
          case 0 => subs.map(_.value).sum
          case 1 => subs.map(_.value).product
          case 2 => subs.map(_.value).min
          case 3 => subs.map(_.value).max
          case 5 => if subs.head.value > subs(1).value then 1 else 0
          case 6 => if subs.head.value < subs(1).value then 1 else 0
          case 7 => if subs.head.value == subs(1).value then 1 else 0
          case _ => throw new Exception

@main
def day16(): Unit = day[16] {
  val Type0Length = 15
  val Type1Length = 11
  val mapping     = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111"
  )

  val parsed = input.head.flatMap(mapping)

  def zeroesToDrop(s: String, rest: String, dropZeros: Boolean) =
    if dropZeros then 8 - (s.length - rest.length) % 8 else 0

  def readAll(s: String, dropZeros: Boolean = true): List[Package] =
    if s.nonEmpty
    then read(s).pipe { (readPackage, rest) =>
        readPackage :: readAll(rest.drop(zeroesToDrop(s, rest, dropZeros)), dropZeros)
      }
    else Nil

  def readLiteral(s: String, version: Long) =
    literalNumber(s.drop(6)).pipe((result, rest) => Package.Literal(version, result.binary) -> rest)

  def operatorLengthType(s: String) = s.slice(6, 7)

  def readOperatorType0(s: String, version: Long, packageType: Long, length: Int) =
    readAll(s.slice(7 + Type0Length, 7 + Type0Length + length), dropZeros = false)
      .pipe(Package.Operator(version, packageType, _))
      .pipe(operator => (operator, s.drop(7 + length + Type0Length)))

  def readOperatorType1(s: String, version: Long, packageType: Long, length: Int) =
    (1 to length)
      .foldLeft((List.empty[Package], s.drop(7 + Type1Length))) {
        case ((found, rest), _) => read(rest).pipe((next, rest) => (found :+ next, rest))
      }
      .pipe((subs, rest) => Package.Operator(version, packageType, subs) -> rest)

  def readOperator(s: String, version: Long, pType: Long) =
    if operatorLengthType(s) == "0"
    then readOperatorType0(s, version, pType, s.slice(7, 7 + Type0Length).binaryInt)
    else readOperatorType1(s, version, pType, s.slice(7, 7 + Type1Length).binaryInt)

  def version(s: String): Long = s.take(3).binary

  def packageType(s: String): Long = s.slice(3, 6).binary

  def read(s: String): (Package, String) =
    if packageType(s) == 4
    then readLiteral(s, version(s))
    else readOperator(s, version(s), packageType(s))

  def literalNumber(s: String): (String, String) =
    if s.startsWith("1")
    then literalNumber(s.drop(5)).pipe((result, rest) => (s.slice(1, 5) + result, rest))
    else (s.slice(1, 5), s.drop(5))

  def result = readAll(parsed)

  part[1](result.map(_.versionSum).sum)
  part[2](result.head.value)

}
