package aoc

import scala.util.chaining.*

@main
def day8(): Unit =
  val in = readResourceLines("day8.txt").map { case s"$l | $r" =>
    def parse(s: String) =  s.strip().split(' ').toList.map(_.toSet)
    (parse(l), parse(r))
  }.toMap

  in.values.map(_.count(x => x.size != 5 && x.size != 6)).sum.pipe(x => println(s"part 1: $x"))

  val res = in.map { (key, value) =>
    def `0` = key.filter(_.size == 6).find(n => n != `6` && n != `9`).get
    def `1` = key.find(_.size == 2).get
    def `2` = key.filter(_.size == 5).find(n => n != `3` && n ++ `1` != `9`).get
    def `3` = key.filter(_.size == 5).find(n => `1` subsetOf n).get
    def `4` = key.find(_.size == 4).get
    def `6` = key.filter(_.size == 6).find(n => n != `9` && (`4` -- `1`).subsetOf(n)).get
    def `7` = key.find(_.size == 3).get
    def `8` = key.find(_.size == 7).get
    def `9` = key.filter(_.size == 6).find(`4` subsetOf _).get

    val mapping =
      Map(
        `0` -> "0",
        `1` -> "1",
        `2` -> "2",
        `3` -> "3",
        `4` -> "4",
        `6` -> "6",
        `7` -> "7",
        `8` -> "8",
        `9` -> "9"
      ).withDefaultValue("5")

    key -> value.map(_.toSet).map(mapping).mkString.toInt
  }

  println(s"part 2: ${res.values.sum}")
