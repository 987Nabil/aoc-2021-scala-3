package aoc

enum Shape:
  case Rock, Paper, Scissors

  def beats(that: Shape): Boolean = this match
    case Shape.Rock     => that == Shape.Scissors
    case Shape.Paper    => that == Shape.Rock
    case Shape.Scissors => that == Shape.Paper

  def score: Int = this match
    case Shape.Rock     => 1
    case Shape.Paper    => 2
    case Shape.Scissors => 3

final case class Round(elf: Shape, me: Shape):

  def outcomeScore: Int =
    if me beats elf
    then 6
    else if elf beats me
    then 0
    else 3

  def myScore = outcomeScore + me.score

def elfenShape(s: String): Shape = s match
  case "A" => Shape.Rock
  case "B" => Shape.Paper
  case "C" => Shape.Scissors

def myShape(s: String): Shape = s match
  case "X" => Shape.Rock
  case "Y" => Shape.Paper
  case "Z" => Shape.Scissors

def myShape2(s: String, elfenShape: Shape): Shape = s match
  case "X" => Shape.values.find(s => elfenShape beats s).get
  case "Y" => elfenShape
  case "Z" => Shape.values.find(_ beats elfenShape).get

@main
def day2 = day[2] {

  part[1] {
    input.map { case s"$elf $me" => Round(elfenShape(elf), myShape(me)) }.map(_.myScore).sum
  }

  part[2] {
    input.map { case s"$elf $me" => Round(elfenShape(elf), myShape2(me, elfenShape(elf))) }.map(_.myScore).sum
  }
}
