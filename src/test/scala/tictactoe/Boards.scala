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
   * This is not all winning boards though, just the ones where a player wins with a minimum number of moves
   */
  def winningBoards = Gen.oneOf(player1WinsBoards, player2WinsBoards).flatMap(b => symmetries(b))

  /** generator for all the possible symmetries of a board */
  def symmetries = (b: Board) => Gen.oneOf(Symmetry.symmetries.map(b.transform))

  /** boards where player1 wins */
  def player1WinsBoards: Gen[Board] = for {
    // a winning line for player 1
    player1Moves  <- winningSeqs
    // 2 moves for player 2, which can't be the same as the ones for player1
    player2Moves  <- moveSeqs(2, 2).filter(mvs => !mvs.exists(player1Moves.contains))
  } yield Board(zipIn(player1Moves, player2Moves):_*)

  /** boards where player2 wins */
  def player2WinsBoards: Gen[Board] = for {
    player2Moves  <- winningSeqs
    player1Moves  <- moveSeqs(3, 3).filter(mvs => !mvs.exists(player2Moves.contains) && !Board.isWinning(mvs))
  } yield Board(zipIn(player1Moves, player2Moves):_*)

  /** minimum sequences of 3 moves making a winning line, the rest being computable by a symmetry */
  def winningSeqs: Gen[Seq[Move]] = Gen.oneOf(Board.minimumWinLines)

  /** any random move */
  def moves = Gen.oneOf(Move.allMoves)

  /** any possible sequence of moves of an arbitrary size */
  def moveSeqs(min: Int = 0, max: Int = 9) =
    ^(Gen.oneOf(allSequences), Gen.choose(min, max))((mvs: List[Move], size: Int) => mvs.take(size))

  /** this allows using the apply notation with generators */
  implicit def genIsApply: Apply[Gen] = new Apply[Gen] {
    def ap[A, B](fa: => Gen[A])(f: => Gen[(A) => B]) = fa.map2(f)((v, function) => function(v))
    def map[A, B](fa: Gen[A])(f: (A) => B) = fa map f
  }

  /**
   * intercalate the elements of a list with another
   *
   * If the second list has less elements, add dummy elements to make sure that it's possible to intercalate a list with
   * n - 1 elements into a list with n elements
   */
  def zipIn[T](seq1: Seq[T], seq2: Seq[T]) =
    if (seq2.size < seq1.size) seq1.zip(seq2 :+ (null.asInstanceOf[T])).flatMap { case (x, y) => Seq(x, y) }.dropRight(1)
    else                       seq1.zip(seq2).flatMap { case (x, y) => Seq(x, y) }
}
