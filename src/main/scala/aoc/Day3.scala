package aoc

@main
def day3(): Unit =
  val bits = readResourceLines("day3.txt").map(_.map(_.toString.toInt))
  val commonBits = bits.transpose.map(x => (mostCommonBit(x), leastCommonBit(x)))

  val (gamma, epsilon) =
    commonBits.reverse.zipWithIndex
      .foldLeft((0, 0)) { case ((gamma, epsilon), ((mcb, lcb), i)) =>
        (gamma + (mcb << i), epsilon + (lcb << i))
      }

  val (Seq(oxygen), Seq(co2)) =
    commonBits.indices.foldLeft((bits, bits)) {
      case ((oxygenBitsLeft, co2BitsLeft), i) =>
        val mcb = oxygenBitsLeft.transpose.map(mostCommonBit)(i)
        val lcb = co2BitsLeft.transpose.map(leastCommonBit)(i)
        (
          if oxygenBitsLeft.size > 1 then oxygenBitsLeft.filter(_ (i) == mcb) else oxygenBitsLeft,
          if co2BitsLeft.size > 1 then co2BitsLeft.filter(_ (i) == lcb) else co2BitsLeft
        )
    }

  println(s"part 1: ${gamma * epsilon}")
  println(s"part 2: ${toBinary(oxygen) * toBinary(co2)}")

def mostCommonBit(bits: Seq[Int]): Int =
  if bits.count(_ == 1) >= bits.count(_ == 0) then 1 else 0

def leastCommonBit(bits: Seq[Int]) =
  if bits.count(_ == 1) < bits.count(_ == 0)then 1 else 0

def toBinary(bits: Seq[Int]): Int =
  bits.reverse.zipWithIndex.foldLeft(0) { case (acc, (bit, i)) => acc + (bit << i) }
