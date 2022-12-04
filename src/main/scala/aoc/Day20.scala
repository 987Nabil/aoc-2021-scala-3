package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day20(): Unit = day[20] {

  val replacements: Map[Int, Char] = input.head.zipWithIndex.map(_.swap).toMap

  def toBinaryMap(s: Seq[String]) =
    s.map(_.toSeq.collect { case '.' => 0; case '#' => 1 })
      .pipe { map =>
        for
          x <- map.head.indices
          y <- map.indices
        yield (x, y) -> map.point(x, y)
      }
      .toMap
      .withDefaultValue(0)

  val inputImage: CoordMap = toBinaryMap(input.tail.tail)

  def processImage(inputImage: CoordMap) = {
    for
      pixel      <- inputImage.keySet.flatMap(_.neighbours)
      replacement = pixel.neighbours.map(inputImage).mkString.pipe(_.binaryInt).pipe(replacements)
    yield pixel -> replacement
  }.toList
    .sortBy(_._1.x)
    .sortBy(_._1.y)
    .tap(x => println(x.tap(x => println(x.count(_._2 == '#')))))

  (1 to 50)
    .foldLeft(inputImage) { (inputImage, i) =>
      processImage(inputImage)
        .groupMapReduce(_._1.y)(_._2.toString)(_ + _)
        .toSeq
        .sortBy(_._1)
        .map(_._2)
        .tap(x => println(x.mkString("\n")))
        .pipe(toBinaryMap)
        .withDefaultValue(i % 2)
    }
    .values
    .count(_ == 1)
    .tap(println)

}
