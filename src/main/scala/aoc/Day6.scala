package aoc

@main
def day6(): Unit =
  val fish = readResourceLines("day6.txt").head.split(',').toList.map(_.toInt)

  val part1 =
    (1 to 80)
      .foldLeft(fish) { case (fish, _) =>
        fish.flatMap {
          case 0 => 6 :: 8 :: Nil
          case x => x - 1 :: Nil
        }
      }
      .size

  val part2 =
    (1 to 256)
      .foldLeft((0 to 8).map(i => i -> fish.count(_ == i).toLong).toMap) { case (lastDayFish, _) =>
        val todayFishNotDeliveringOrNewBorn = (for i <- 1 to 8 yield i - 1 -> lastDayFish(i)).toMap

        val fishDeliveringNextWeek = 6 -> (todayFishNotDeliveringOrNewBorn(6) + lastDayFish(0))
        val newBornFish            = 8 -> lastDayFish(0)

        todayFishNotDeliveringOrNewBorn + fishDeliveringNextWeek + newBornFish
      }
      .values
      .sum

  println(s"part 1: $part1")
  println(s"part 2: $part2")
