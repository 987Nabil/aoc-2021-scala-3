package aoc

@main
def day6(): Unit =
  val fish = fishCountByDueDate(readResourceLines("day6.txt").head.split(',').toList.map(_.toInt))

  println(s"part 1: ${fishPopulationAfter(fish, daysToObserve = 80)}")
  println(s"part 2: ${fishPopulationAfter(fish, daysToObserve = 256)}")

def fishPopulationAfter(fish: Map[Int, Long], daysToObserve: Int): Long =
  (1 to daysToObserve)
    .foldLeft(fish) { case (lastDayFish, _) =>
      todayFishNotDeliveringOrNewBorn(lastDayFish)
        + fishDeliveringNextWeek(lastDayFish)
        + newBornFish(lastDayFish)
    }
    .values
    .sum

def fishCountByDueDate(fish: List[Int]): Map[Int, Long] =
  (0 to 8).map(i => i -> fish.count(_ == i).toLong).toMap

def todayFishNotDeliveringOrNewBorn(lastDayFish: Map[Int, Long]): Map[Int, Long] =
  (for i <- 1 to 8 yield i - 1 -> lastDayFish(i)).toMap

def fishDeliveringNextWeek(lastDayFish: Map[Int, Long]): (Int, Long) =
  6 -> (todayFishNotDeliveringOrNewBorn(lastDayFish)(6) + lastDayFish(0))

def newBornFish(lastDayFish: Map[Int, Long]): (Int, Long) =
  8 -> lastDayFish(0)
