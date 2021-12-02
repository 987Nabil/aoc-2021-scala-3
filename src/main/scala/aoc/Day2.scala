package aoc

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.getClass
import scala.util.Random

enum CommandType:
  case Down, Up, Forward

case class Command(commandType: CommandType, value: Int)

@main
def day2(): Unit =
  val Input = """([a-z]*)\s*([0-9]*)""".r
  val commandsAndValues: Seq[Command] =
    readResourceLines("day2.txt")
      .map { case Input(command, value) =>
        Command(CommandType.valueOf(command.capitalize), value.toInt)
      }

  println(s"part 1 ${part1(commandsAndValues)}")
  println(s"part 2: ${part2(commandsAndValues)}")

def part1(commands: Seq[Command]) =
  val sumOfCommands = commands.groupMap(_.commandType)(_.value).view.mapValues(_.sum)
  sumOfCommands(CommandType.Forward) *
    (sumOfCommands(CommandType.Down) - sumOfCommands(CommandType.Up))

def part2(commands: Seq[Command]) =
  val (aim, depth, hPos) = commands.foldLeft((0, 0, 0)) {
    case ((aim, depth, hPos), Command(cType, value)) =>
      cType match {
        case CommandType.Forward => (aim, depth + aim * value, hPos + value)
        case CommandType.Down    => (aim + value, depth, hPos)
        case CommandType.Up      => (aim - value, depth, hPos)
      }
  }
  depth * hPos
