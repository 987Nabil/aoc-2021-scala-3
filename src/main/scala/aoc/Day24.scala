package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}
import scala.util.Random

enum Variable:
  case W, X, Y, Z

enum OperationType:
  case Add, Mul, Div, Mod, Eql

  def calc(v1: Variable, v2: Variable | Int, state: Map[Variable, Int]): Map[Variable, Int] =
    state + {
      v1 -> {
        this match
          case Add => state(v1) + state.value(v2)
          case Mul => state(v1) * state.value(v2)
          case Div => state(v1) / state.value(v2)
          case Mod => state(v1) % state.value(v2)
          case Eql => if state(v1) == state.value(v2) then 1 else 0
      }
    }

enum ALUInstruction:
  case Operation(op: OperationType, v1: Variable, v2: Variable | Int)
  case Input(v: Variable)

extension (s: Map[Variable, Int])
  def value(v: Variable | Int): Int =
    v match
      case v: Variable => s(v)
      case i: Int      => i

@main
def day24(): Unit = day[24] {
  val instructions: Seq[ALUInstruction] = input.map {
    case s"inp $v"      => ALUInstruction.Input(Variable.valueOf(v.capitalize))
    case s"$op $v1 $v2" =>
      ALUInstruction.Operation(
        OperationType.valueOf(op.capitalize),
        Variable.valueOf(v1.capitalize),
        v2.toIntOption.getOrElse(Variable.valueOf(v2.capitalize)),
      )
  }

  @tailrec
  def monad(
      inputs: Seq[Int],
      instructions: Seq[ALUInstruction],
      lastV: Option[Variable],
      state: Map[Variable, Int],
      result: String,
    ): Long =
    if instructions.isEmpty && state(Variable.Z) == 0 then
      ((if state(lastV.get) % 10 == 0 then 1 else state(lastV.get) % 10).toString + result).toLong
    else if instructions.isEmpty then 0
    else
      instructions.head match
        case ALUInstruction.Input(v)              =>
          monad(
            inputs.tail,
            instructions.tail,
            Some(v),
            state + (v -> inputs.head),
            lastV.flatMap(v => state.get(v)).fold(result)(_.toString + result),
          )
        case ALUInstruction.Operation(op, v1, v2) =>
          monad(inputs, instructions.tail, lastV, op.calc(v1, v2, state), result)

  Iterator
    .fill(100)(Seq.fill(14)(Random.nextInt(8)+1))
    .flatMap(_.permutations.tap(_ => println("NEXT")))
    .map {
      monad(
        _,
        instructions,
        None,
        Variable.values.toSeq.map(_ -> 0).toMap,
        "",
      )
    }
    .find(_ != 0)
    .tap(println)

}
