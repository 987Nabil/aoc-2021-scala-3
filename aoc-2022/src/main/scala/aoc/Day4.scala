package aoc

@main
def day4(): Unit = day[4] {
  val sections = input.map { case s"$s1n1-$s1n2,$s2n1-$s2n2" =>
    (s1n1.toInt to s1n2.toInt, s2n1.toInt to s2n2.toInt)
  }

  part[1] {
    sections.count { case (s1, s2) => s1.containsSlice(s2) || s2.containsSlice(s1) }
  }
  part[2] {
    sections.count { case (s1, s2) => s1.exists(s2.contains) }
  }
}
