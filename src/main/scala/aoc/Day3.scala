package aoc

@main
def day3(): Unit =
  val bits = readResourceLines("day3.txt").map(_.map(_.toString.toInt))

  val gamma = Integer.parseInt(bits.transpose.map(mostCommonBit).mkString, 2)
  val epsilon = Integer.parseInt(bits.transpose.map(leastCommonBit).mkString, 2)

  val (Seq(oxygen), Seq(co2)) =
    bits.head.indices.foldLeft((bits, bits)) {
      case ((oxygenBitsLeft, co2BitsLeft), i) =>
        val mcb = oxygenBitsLeft.transpose.map(mostCommonBit)(i)
        val lcb = co2BitsLeft.transpose.map(leastCommonBit)(i)
        (
          if oxygenBitsLeft.size > 1 then oxygenBitsLeft.filter(_ (i) == mcb) else oxygenBitsLeft,
          if co2BitsLeft.size > 1 then co2BitsLeft.filter(_ (i) == lcb) else co2BitsLeft
        )
    }

  println(s"part 1: ${gamma * epsilon}")
  println(s"part 2: ${Integer.parseInt(oxygen.mkString, 2) * Integer.parseInt(co2.mkString, 2)}")

def mostCommonBit(bits: Seq[Int]): Int =
  if bits.count(_ == 1) >= bits.count(_ == 0) then 1 else 0

def leastCommonBit(bits: Seq[Int]) =
  if bits.count(_ == 1) < bits.count(_ == 0) then 1 else 0
