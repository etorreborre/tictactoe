package tictactoe

import org.specs2.{Specification, ScalaCheck}
import org.specs2.matcher.Matcher
import org.scalacheck.{Gen, Arbitrary}
import tictactoe.Board._
import scalaz.Scalaz._
import scalaz.Apply

/**
 * this trait generates arbitrary moves and boards
 */
trait Boards extends ScalaCheck { this: Specification =>

  /** @return a matcher to check if a Board is full */
  def beFull: Matcher[Board] = (b: Board) => (b.isFull, b+" is not full")

  implicit val arbitraryBoards: Arbitrary[Board] = Arbitrary(boards)
  implicit val arbitraryMoves: Arbitrary[Move]   = Arbitrary(moves)

  def boards        = Gen.oneOf(emptyBoards, winningBoards, randomBoards)
  def emptyBoards   = Gen.value(Board.empty)
  def randomBoards  = moveSeqs(0, 9).map(Board.create)

  /**
   * winning boards are created by interspersing a winning sequence of moves (a "line") with an arbitrary sequence.
   * This is not all winning boards though and the board might not even be valid!
   */
  def winningBoards = Gen.oneOf(player1WinsBoards, player2WinsBoards)

  /** boards where player1 wins */
  def player1WinsBoards: Gen[Board] = for {
    player1Moves  <- winningSeq
    player2Moves  <- moveSeqs(3, 3)
  } yield Board(player1Moves.zip(player2Moves).flatMap { case (p1, p2) => Seq(p1, p2) }.dropRight(1):_*)

  /** boards where player2 wins */
  def player2WinsBoards: Gen[Board] = for {
    player1Moves  <- moveSeqs(3, 3).filter(mvs => !Board.isWinning(mvs))
    player2Moves  <- winningSeq
  } yield Board(player1Moves.zip(player2Moves).flatMap { case (p1, p2) => Seq(p1, p2) }:_*)

  /** any sequence of 3 moves making a winning line */
  def winningSeq = Gen.choose(0, 7).map(Board.winningMoves)

  /** any random move */
  def moves = Gen.oneOf(allMoves)

  /** any possible sequence of moves of an arbitrary size */
  def moveSeqs(min: Int = 0, max: Int = 9) =
    ^(Gen.oneOf(allSequences), Gen.choose(min, max))((mvs: List[Move], size: Int) => mvs.take(size))

  /** this allows using the apply notation with generators */
  implicit def genIsApply: Apply[Gen] = new Apply[Gen] {
    def ap[A, B](fa: => Gen[A])(f: => Gen[(A) => B]) = fa.map2(f)((v, function) => function(v))
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
  }
}
