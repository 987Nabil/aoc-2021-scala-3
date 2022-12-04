package aoc

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.{MapView, mutable}

@main
def day21(): Unit = day[21] {
  val playerOneStart = input.head.last.asDigit
  val playerTwoStart = input.tail.head.last.asDigit

  def deterministicDice = Iterator.continually(Iterator.range(1, 101)).flatten

  def movePlayer(player: (Int, Int), moves: Int) =
    player.pipe((score, pos) =>
      ((moves + pos) % 10)
        .pipe(pos => if pos == 0 then 10 else pos)
        .pipe(pos => (score + pos, pos))
    )

  @tailrec
  def play(
      dice: Iterator[Int],
      playerOne: (Int, Int),
      playerTwo: (Int, Int),
      timesRolled: Int,
      maxScore: Int,
      playOnePlays: Boolean = true
    ): (Either[(Int, Int), (Int, Int)], Int) =
    if playerOne._1 >= maxScore then (Right(playerTwo), timesRolled)
    else if playerTwo._1 >= maxScore then (Left(playerOne), timesRolled)
    else
      val (rolls, rest) = dice.splitAt(3)
      play(
        rest,
        if playOnePlays then movePlayer(playerOne, rolls.sum) else playerOne,
        if !playOnePlays then movePlayer(playerTwo, rolls.sum) else playerTwo,
        timesRolled + 6,
        maxScore,
        !playOnePlays,
      )

  val dicesRolls =
    for
      i <- 1 to 3
      j <- 1 to 3
      k <- 1 to 3
    yield i + j + k

  var cache: Map[((Int, Int), (Int, Int), Boolean), (Long, Long)] = Map.empty

  def play2(
      playerOne: (Int, Int),
      playerTwo: (Int, Int),
      maxScore: Int,
      playerOnePlays: Boolean = true
    ): (Long, Long) =
    if playerOne._1 >= maxScore then (1, 0)
    else if playerTwo._1 >= maxScore then (0, 1)
    else
      val res: (Long, Long) = cache
        .get((playerOne, playerTwo, playerOnePlays))
        .fold[(Long, Long)] {
          dicesRolls.foldLeft((0L,0L)) { case ((scoreOne, scoreTwo) , rolls) =>
            play2(
              if playerOnePlays then movePlayer(playerOne, rolls) else playerOne,
              if !playerOnePlays then movePlayer(playerTwo, rolls) else playerTwo,
              maxScore,
              !playerOnePlays,
            ).pipe((sOne, sTwo) => (scoreOne + sOne, scoreTwo + sTwo))
          }
        }(identity)

      cache += (playerOne, playerTwo, playerOnePlays) -> res
      res

  part[1](
    play(deterministicDice, (0, playerOneStart), (0, playerTwoStart), 0, 1000).pipe(
      (looser, timesRolled) => looser.toOption.orElse(looser.swap.toOption).get._1 * timesRolled
    )
  )
  part[2](play2((0, playerOneStart), (0, playerTwoStart), 21).toList.max)

}
