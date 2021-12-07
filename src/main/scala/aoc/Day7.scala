package aoc

@main
def day7(): Unit =
  val positions = readResourceLines("day7.txt").head.split(',').map(_.toInt).toList
  val costs     = cheapestCosts(positions.min to positions.max)

  val (optPos, fuelUsed)   = costs(crabMarineFuelCosts1(positions))
  val (optPos2, fuelUsed2) = costs(crabMarineFuelCosts2(positions))

  println(s"part 1: $fuelUsed")
  println(s"part 2: $fuelUsed2")

def cheapestCosts(alignments: Range)(costs: Int => Int) =
  alignments.foldLeft((-1, Int.MaxValue)) { case (old @ (_, minCost), alignTo) =>
    if minCost <= costs(alignTo) then old else (alignTo, costs(alignTo))
  }

def crabMarineFuelCosts1(positions: List[Int])(alignTo: Int): Int =
  positions.foldLeft(0)((usedFuel, nextPos) => usedFuel + (nextPos - alignTo).abs)

def crabMarineFuelCosts2(positions: List[Int])(alignTo: Int) =
  positions.map { nextPos => (1 to (nextPos - alignTo).abs).sum }.sum
