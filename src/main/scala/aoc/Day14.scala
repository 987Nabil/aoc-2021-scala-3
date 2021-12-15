package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day14(): Unit = day[14] {

  val in         = input.head
  val insertions = input.drop(2).map { case s"$k -> $v" => (k.head, k.last) -> v.head }.toMap

  extension (m: Map[Char, Long])
    infix def merge(that: Map[Char, Long]): Map[Char, Long] =
      (m.toSeq ++ that.toSeq).groupMapReduce(_._1)(_._2)(_ + _)

  type Cache = Map[(Int, Char, Char), Map[Char, Long]]

  def polymerization(depth: Int)(cache: Cache, in: (Char, Char)): (Map[Char, Long], Cache) =
    if depth == 0 then (Map(in._1 -> 1L) merge Map(in._2 -> 1L), cache)
    else
      cache
        .get(depth *: in)
        .fold {
          val (left, lCache)   = polymerization(depth - 1)(cache, (in._1, insertions(in)))
          val (right, rCache)  = polymerization(depth - 1)(lCache, (insertions(in), in._2))
          val merged = left merge right merge Map(insertions(in) -> -1)
          (merged, rCache + (depth *: in -> merged))
        }(_ -> cache)

  lazy val one = in
    .zip(in.tail)
    .scanLeft((Map.empty[Char, Long], Map.empty: Cache)){case ((s,c), in) => polymerization(10)(Map.empty, in)}
    .map(_._1)
    .reduce(_ merge _) merge in.init.tail.map(_ -> -1L).groupMapReduce(_._1)(_._2)(_ + _)

  lazy val two = in
    .zip(in.tail)
    .scanLeft((Map.empty[Char, Long], Map.empty: Cache)){case ((s,c), in) => polymerization(40)(Map.empty, in)}
    .map(_._1)
    .reduce(_ merge _) merge in.init.tail.map(_ -> -1L).groupMapReduce(_._1)(_._2)(_ + _)

  part[1](one.values.max - one.values.min)
  part[2](two.values.max - two.values.min)

}
