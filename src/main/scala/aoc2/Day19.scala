package aoc2

import scala.collection.View
import scala.math.sqrt

type VectorPoint = (BigInt, BigInt, BigInt)

object VectorPoint:
  val Deg90  = (BigInt(-1), BigInt(1), BigInt(1))
  val Deg180 = (BigInt(-1), BigInt(-1), BigInt(1))
  val Deg270 = (BigInt(1), BigInt(-1), BigInt(1))

type Report = Seq[VectorPoint]

extension (n: BigInt)
  def sqrt: BigInt = {
    var a = BigInt(1)
    var b = (n >> 5) + BigInt(8)
    while ((b - a) >= 0) {
      var mid: BigInt = (a + b) >> 1
      if (mid * mid - n > 0) b = mid - 1
      else a = mid + 1
    }
    a - 1
  }

extension (self: VectorPoint)
  def x: BigInt                               = self._1
  def y: BigInt                               = self._2
  def z: BigInt                               = self._3
  inline def *(other: VectorPoint)            = (self.x * other.x, self.y * other.y, self.z * other.z)
  inline def +(other: VectorPoint)            = (self.x + other.x, self.y + other.y, self.z + other.z)
  inline def -(other: VectorPoint)            = (self.x - other.x, self.y - other.y, self.z - other.z)
  def rotate1                                 = (-self.x, -self.z, -self.y)
  def rotate2                                 = (-self.z, -self.y, self.z)
  def rotate3                                 = (self.y, -self.x, self.z)
  def rotate4                                 = (-self.y, -self.z, -self.x)
  def length: BigInt                          = (self.x.pow(2) + self.y.pow(2) + self.z.pow(2)).sqrt
  def normalize: VectorPoint                  =
    ((self.x / self.length).pow(2), (self.y / self.length).pow(2), (self.z / self.length).pow(2))
  infix def |---|(other: VectorPoint): BigInt =
    ((self.x - other.x).pow(2) + (self.y - other.y).pow(2) + (self.z - other.z).pow(2)).sqrt


import java.io.{File, PrintWriter}
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Definitions:
  private[Definitions] var testMode = false

  type PartNumber = 1 | 2
  private type DayNumberHelper[N <: Int] <: Int = N match
    case 1 => N
    case _ => N | DayNumberHelper[N - 1]
  type DayNumber = DayNumberHelper[25]

  opaque type Input <: String = String
  type Output = Any

  private inline def dayValue[N <: DayNumber]: N = constValue[N]

  trait Solution[N <: DayNumber]:
    def apply(input: Input, part: Part[N]): Unit

  val lineSeparator = "\n"
  extension (input: Input) def toLines: IndexedSeq[String] = input.split("\r?\n").toIndexedSeq

  final class Part[N <: DayNumber](day: N):
    import scala.collection.mutable
    private[Definitions] var last: Int = 0
    def update(part: PartNumber, v: => Output): Unit =
      require(part == last + 1)
      last = part
      Try(v).map(String.valueOf) match
        case Success(output) =>
          println(output)
          if !testMode then
            writeOutput(output, day, part)
          else
            val expected = Source.fromFile(pathForOutput(day, part)).mkString
            assert(output == expected, s"Day $day part $part: output '$output' didn't match '$expected'")
        case Failure(e: NotImplementedError) =>
          println(s"(ignored part $part)")
        case Failure(e) => throw new Exception(e)

  inline def Day[N <: DayNumber](day: N)(implementation: Solution[N]): Unit = implementation(readInput(day), new Part(constValue[N]))

  private def formatDay(day: DayNumber): String = f"$day%02d"

  private val (inputDirectory, outputDirectory) = ("input", "output")
  private def pathForInput(day: DayNumber): String =
    s"src/main/resources/day${formatDay(day)}.txt"
  private def pathForOutput(day: DayNumber, part: PartNumber): String =
    s"$outputDirectory/${formatDay(day)}-$part.txt"

  private def readInput(day: DayNumber): Input =
    Source.fromFile(pathForInput(day)).mkString

  private def writeOutput(output: String, day: DayNumber, part: PartNumber): Unit =
    val path = pathForOutput(day, part)
    new File(path).getParentFile.mkdirs()
    new PrintWriter(path):
      write(output)
      close()
import Definitions.*
@main
def day19(): Unit = Day(19) { (input, part) =>
//
//  def parseAll(lines: Seq[String]): Seq[Report] =
//    lines.span(!_.startsWith("---")).pipe { (matrixLines, rest) =>
//      matrixLines.map { case s"$x,$y,$z" =>
//        (BigInt(x), BigInt(y), BigInt(z))
//      } +: (if rest.isEmpty then Nil else parseAll(rest.tail))
//    }
//
//  val reports = parseAll(input.filterNot(_.isBlank).tail)
//
////  def overlapping(
////      r1Distances: Seq[((VectorPoint, VectorPoint), BigInt)],
////      r2Distances: Seq[((VectorPoint, VectorPoint), BigInt)]
////    ): Seq[VectorPoint] =
////    def recur(
////        xx: Seq[((VectorPoint, VectorPoint), BigInt)],
////        yy: Seq[((VectorPoint, VectorPoint), BigInt)]
////      ): Seq[VectorPoint] =
////      if xx.isEmpty then Nil else
////        val ((r1p1, r1p2), r1Distance) = xx.tail.tail.head.tap(println)
////        val projection = yy.find(_._2 == r1Distance)
////        projection.fold(Nil) { case ((r2p1, r2p2), int) =>
////          List(r1p1, r1p2) ++ recur(r1Distances.filter(_._1._1 == r1p2).tap(println), r2Distances.filter(_._1._1 == r2p2).tap(println))
////        }
////
////    val distancesSet1 = r1Distances.map(_._2).toSet
////    val distancesSet2 = r2Distances.map(_._2).toSet
////    val commonDistances = distancesSet1 & distancesSet2
////    recur(r1Distances.filter(x => commonDistances(x._2)),r2Distances.filter(x => commonDistances(x._2)))
//
////  reports
////    .map(_.combinations(2).flatMap { case Seq(a, b) => List((a, b) -> a.distance(b), (b, a) -> a.distance(b)) }.toSeq.sortBy(_._2))
////    .pipe { case Seq(one, two) => overlapping(one, two) }
////    .tap(println)
//
//  val xx = reports.map(x =>
//    x.map(y =>
//      y -> x
//        .filterNot(_ == y)
//        .foldLeft(Seq.empty[(VectorPoint, BigInt)])((acc, next) => (next -> (y |---| next)) +: acc)
//    )
//  )
//  xx.combinations(2)
//    .map { case Seq(first, second) =>
//      for
//        (point, distances)           <- first
//        (otherPoint, otherDistances) <- second
//        commonDistances               = distances.map(_._2).toSet & otherDistances.map(_._2).toSet
//        if commonDistances.size == 11
//      yield (
//        point +: distances.filter(x => commonDistances(x._2)).sortBy(_._2).map(_._1),
//        otherPoint +: otherDistances
//          .filter(x => commonDistances(x._2))
//          .sorted
//          .sortBy(_._2)
//          .map(_._1)
//      )
//    }
//    .toList
//    .collect {
//      case that if that.nonEmpty =>
//        val one       = that.map(_._1).reduce(_ intersect _)
//        one.mkString("\n").tap(println)
//        println
//        val two       = that.map(_._2).reduce(_ intersect _)
//        two.mkString("\n").tap(println)
//        println
//        (one.head - two.head.rotate1).tap(println)
//        (one.head - two.head.rotate2).tap(println)
//        (one.head - two.head.rotate3).tap(println)
//        (one.head - two.head.rotate4).tap(println)
//        val rotations = List(1, 1, 1, -1, -1, -1).permutations
//          .map(_.take(3).pipe { case Seq(a, b, c) => (BigInt(a), BigInt(b), BigInt(c)) })
//          .toSet
//        val zipped =
//        rotations.map(x => one.zip(two.map(_ * x)).map(_ - _).toSet).find(_.size == 1).tap(println)
//    }
//    .tap(println)
//    .size
//    .tap(println)
//
//  reports.map(_.size).tap(println).sum.tap(println)
//
//  //  (for
////    combi1 <- reports.head.combinations(12)
////    combi1Distances = combi1.sliding(2).map{case Seq(a,b) => a.distance(b)}
////    combi2 <- reports.last.combinations(12)
////    combi2Distances = combi1.sliding(2).map{case Seq(a,b) => a.distance(b)}
////    if combi1Distances.sameElements(combi2Distances)
////  yield combi1 -> combi2).foreach(_.pipe((a,b) => s"${a.mkString("\n")}\n----\n${b.mkString("\n")}\nNEXT\n").tap(println))
//
//  // println(s"reports = \n${reports.mkString("\n")}")
//
//  part[1](0)
//  part[2](0)

  case class Vec(x: Int, y: Int, z: Int):
    def apply(i: Int): Int = i match
      case 0 => x
      case 1 => y
      case 2 => z
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
    def -(that: Vec): Vec = Vec(x - that.x, y - that.y, z - that.z)
    def abs: Int = x.abs + y.abs + z.abs
  object Vec:
    val arity: Int = 3
  val Zero = Vec(0, 0, 0)

  val points = input.split("\n\n").map(_.split("\n").tail.map {
    case s"$x,$y,$z" => Vec(x.toInt, y.toInt, z.toInt)
  }.toVector).toVector

  case class Permutation(permutation: Seq[(Int, Int)]):
    def apply(p: Vec): Vec =
      permutation.map { case (i, s) => p(i) * s } match
        case Seq(x, y, z) => Vec(x, y, z)
    def inverse: Permutation = Permutation(permutation.zipWithIndex.sortBy { case ((v, _), _) => v }.map { case ((_, s), i) => (i, s) })

  val permutations =
    Seq.fill(Vec.arity)(Seq(-1, 1)).flatten.combinations(Vec.arity).flatMap(_.permutations)
      .flatMap(signs => (0 until Vec.arity).permutations.map(permutation => Permutation(permutation.zip(signs)))).toIndexedSeq

  case class Transformation(offset: Vec, permutation: Permutation):
    def apply(p: Vec): Vec = permutation(p) + offset
    def inverse: Transformation =
      val inversed = permutation.inverse
      Transformation(inversed(Zero - offset), inversed)

  def search(remaining: View[(Int, Int)], representation: Map[(Int, Int), Transformation]): Map[(Int, Int), Transformation] =
    remaining.headOption match
      case Some((i, j)) =>
        val (pointsI, pointsJ) = (points(i), points(j))
        val result = permutations.indices.view.flatMap { k =>
          val permutation = permutations(k)
          val pointsJPermuted = pointsJ.map(permutation.apply)
          val exhaustive =
            for
              vi <- pointsI.indices.view
              vj <- pointsJPermuted.indices.view
            yield (vi, vj)
          exhaustive.flatMap { case (vi, vj) =>
            val relative = pointsI(vi) - pointsJPermuted(vj)
            val transformedJ = pointsJPermuted.map(_ + relative)
            if pointsI.toSet.intersect(transformedJ.toSet).sizeIs >= 12 then
              Some(Transformation(relative, permutation))
            else
              None
          }
        }.headOption

        val newRepresentation = result match
          case Some(transform) =>
            representation +
              ((j, i) -> transform) +
              ((i, j) -> transform.inverse)
          case _ => representation
        search(remaining.tail, newRepresentation)
      case _ => representation

  val initialRemaining =
    for
      i <- points.indices.view
      j <- points.indices.view.drop(i + 1).view
    yield (i, j)

  val transformations = search(initialRemaining, Map.empty)

  val graph: Map[Int, Set[(Int, Transformation)]] =
    transformations.groupBy { case ((u, _), _) => u }.view.mapValues(_.map { case ((_, v), t) => (v, t) }.toSet).toMap

  def bfs(toVisit: Set[Int], previous: Map[Int, Int]): Map[Int, Int] =
    if toVisit.nonEmpty then
      val next = toVisit.flatMap(i => graph(i).map((j, _) => j).filter(!previous.contains(_)).map(_ -> i)).toMap
      bfs(next.keySet, previous ++ next)
    else
      previous
  val zero = 0
  val paths = bfs(Set(zero), Map(zero -> zero))
  def reconstruct(last: Int, acc: Seq[Int] = Seq.empty): Seq[Int] =
    val newAcc = last +: acc
    paths.get(last) match
      case Some(previous) if previous != last => reconstruct(previous, newAcc)
      case _ => newAcc

  val finalTransformations = points.indices.map { i =>
    val path = reconstruct(i).reverse
    i -> path.zip(path.tail).map { case (u, v) => transformations((u, v)) }
  }.toMap

  val finalPoints = points.indices.flatMap(i => finalTransformations(i).foldLeft(points(i))((ps, t) => ps.map(t.apply)))

  val scanners = points.indices.map(i => finalTransformations(i).foldLeft(Zero)((p, t) => t(p)))

  val distances = initialRemaining.map((i, j) => (scanners(i) - scanners(j)).abs).max

  part(1) = finalPoints.toSet.size

  part(2) = distances

}
