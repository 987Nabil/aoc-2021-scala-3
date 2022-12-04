package aoc

val priority = (('a' to 'z') ++ ('A' to 'Z')).zipWithIndex.toMap.view.mapValues(_ + 1)
@main
def day3() = day[3] {
  part[1] {
    input.map { line =>
      line.splitAt(line.length / 2).pipe { case (a, b) => priority(a.intersect(b).head) }
    }.sum
  }
  part[2] {
    input.sliding(3,3).map(_.reduce(_ intersect _).head.pipe(priority(_))).sum
  }
}
