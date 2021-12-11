package aoc

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

def readResourceLines(s: String): Seq[String] =
  val file = getClass.getClassLoader.getResource(s).toURI
  Files.readAllLines(Path.of(file)).asScala.toSeq.map(_.strip())

extension [T](map: Seq[Seq[T]])
  def point(x: Int, y: Int): T            = map(y)(x)
  def getPoint(x: Int, y: Int): Option[T] = map.lift(y).flatMap(_.lift(x))

extension [T](x: T)
  inline def pipe[X](inline f: T => X) = f(x)
  inline def tap[U](f: T => U): T      = {
    f(x)
    x
  }

type CoordMap = Map[Coord, Int]
type Coord    = (Int, Int)

extension (c: Coord)
  def x = c._1
  def y = c._2

extension (map: CoordMap)
  def neighbour(c: Coord)              = map.get(c).map(_ => c)
  def neighbours(c: Coord): Set[Coord] =
    Set((0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, 1), (1, -1)).flatMap((x, y) =>
      map.neighbour(c.x + x, c.y + y)
    )
