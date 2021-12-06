package aoc

@main
def day6(): Unit =
  val fish = fishCountByDueDate(readResourceLines("day6.txt").head.split(',').toList.map(_.toInt))

  println(s"part 1: ${fishPopulationAfter(fish, daysToObserve = 80)}")
  println(s"part 2: ${fishPopulationAfter(fish, daysToObserve = 256)}")

def fishPopulationAfter(fish: Map[Int, Long], daysToObserve: Int): Long =
  (1 to daysToObserve)
    .foldLeft(fish)((lastDayFish, _) =>
      notDeliveringOrNewBorn(lastDayFish) + justDelivered(lastDayFish) + newBorn(lastDayFish)
    )
    .values
    .sum

def fishCountByDueDate(fish: List[Int]): Map[Int, Long] =
  (0 to 8).map(i => i -> fish.count(_ == i).toLong).toMap

def notDeliveringOrNewBorn(lastDayFish: Map[Int, Long]): Map[Int, Long] =
  lastDayFish.tail.map(_ - 1 -> _).toMap

def justDelivered(lastDayFish: Map[Int, Long]): (Int, Long) =
  6 -> (notDeliveringOrNewBorn(lastDayFish)(6) + lastDayFish(0))

def newBorn(lastDayFish: Map[Int, Long]): (Int, Long) =
  8 -> lastDayFish(0)
