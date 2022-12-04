package aoc

@main
def day1 = day[1]{
  val grouped = input.foldLeft(List(List.empty[Int])){ (acc, i) =>
    if (i == "") Nil :: acc
    else (i.toInt :: acc.head) :: acc.tail
  }
  part[1]{
    grouped.map(_.sum).max
  }
  part[2]{
    grouped.map(_.sum).sorted.takeRight(3).sum
  }
}