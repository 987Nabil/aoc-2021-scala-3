package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day25(): Unit = day[25] {
  val xMax                        = input.head.length
  val yMax                        = input.length
  val cucumbers: Map[Coord, Char] = {
    for
      x <- 0 to xMax
      y <- 0 to yMax
      v <- input.lift(y).flatMap(_.lift(x))
    yield (x, y) -> v
  }.toMap

  extension (map: Map[Coord, Char])
    def eastOf(c: Coord): Char      = map(map.eastCoord(c))
    def southOf(c: Coord): Char     = map(map.southCoord(c))
    def eastCoord(c: Coord): Coord  = if c.x + 1 <= xMax - 1 then (c.x + 1, c.y) else (0, c.y)
    def southCoord(c: Coord): Coord = if c.y + 1 <= yMax - 1 then (c.x, c.y + 1) else (c.x, 0)
  extension (c: Char)
    def facesEast: Boolean  = c == '>'
    def facesSouth: Boolean = c == 'v'
    def isFree: Boolean     = c == '.'

  @tailrec
  def move(map: Map[Coord, Char], steps: Int = 1): Int =
    map.keySet
      .filter(c => map(c).facesEast && map.eastOf(c).isFree)
      .pipe(toEast => map ++ toEast.flatMap(c => Seq(map.eastCoord(c) -> '>', c -> '.')))
      .pipe { map =>
        map.keySet
          .filter(c => map(c).facesSouth && map.southOf(c).isFree)
          .pipe(toSouth => map ++ toSouth.flatMap(c => Seq(map.southCoord(c) -> 'v', c -> '.')))
      }
      .pipe(newMap => if newMap == map then steps else move(newMap, steps + 1))

  move(cucumbers).tap(println)
}
