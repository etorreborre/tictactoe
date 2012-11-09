package tictactoe

import scalaz.Scalaz._
import Players._
import XCoordinates.{Left, Middlex, Right}
import YCoordinates.{Top, Middley, Bottom}
import Board._
import org.specs2.collection.Iterablex._

/**
 * A Board is a sequence of moves played by each player
 * By default the first player is assumed to play with 'o' and the second one with 'x'
 */
case class Board(moves: Move*) {

  /** @return true if there is no more possible moves */
  def isFull = moves.size == 9

  /** make a move, for the current player, on the square represented by x and y coordinates */
  def move(x: XCoordinates.Value, y: YCoordinates.Value): Board = move(Move(x, y))

  /** make several moves for both players */
  def move(mvs: Move*): Board = mvs.foldLeft(this)((b, m) => Board((b.moves :+ m):_*))

  /** @return true if a move can be made on (x, y) */
  def canMove(x: XCoordinates.Value, y: YCoordinates.Value): Boolean = canMove(Move(x, y))
  def canMove(m: Move): Boolean                                      = !moves.contains(m)

  /** @return true if someone has won the game */
  def isWinning: Boolean = winner.isDefined

  /** @return the winner of the game if there is one */
  def winner: Option[Player] =
    if (firstPlayerWins)       Some(Player1)
    else if (secondPlayerWins) Some(Player2)
    else                       None

  override def toString =
    Board.allMoves.sliding(3).map(_.map(show).mkString("|", "|", "|")).mkString("\n", "\n", "\n")

  private def movesForPlayer(i: Int) = moves.zipWithIndex.filter(_._2 % 2 == i).map(_._1)
  private def firstPlayerMoves       = movesForPlayer(0)
  private def secondPlayerMoves      = movesForPlayer(1)

  private def firstPlayerWins       = Board.isWinning(firstPlayerMoves)
  private def secondPlayerWins      = Board.isWinning(secondPlayerMoves)

  private def show(move: Move) =
    moves.zipWithIndex.collect { case (m, i) if m == move => if (i % 2 == 0) "o" else "x" }.headOption.getOrElse(" ")

}

/**
 * utility methods for boards
 */
object Board {
  /** @return a board with no moves at all */
  def empty = Board()
  /** @return a board from a sequence of moves */
  def create(moves: Seq[Move]) = Board(moves:_*)

  /** @return winning horizontal lines */
  def horizontalWins: Seq[Seq[Move]] = YCoordinates.values.toSeq.map(y => XCoordinates.values.toSeq.map(Move(_,y)))
  /** @return winning vertical lines */
  def verticalWins: Seq[Seq[Move]]   = XCoordinates.values.toSeq.map(x => YCoordinates.values.toSeq.map(Move(x,_)))
  /** @return winning diagonal lines */
  def diagonalWins: Seq[Seq[Move]]   = Seq(
    Seq(Move(Left, Top), Move(Middlex, Middley), Move(Right, Bottom)),
    Seq(Move(Left, Bottom), Move(Middlex, Middley), Move(Right, Top)))

  /** @return all possible winning moves */
  def winningMoves: Seq[Seq[Move]]   = horizontalWins ++ verticalWins ++ diagonalWins

  def isWinning(playerMoves: Seq[Move]): Boolean = Board.winningMoves.exists(_.forall(playerMoves.contains))

  lazy val allMoves     = ^(List(Left, Middlex, Right), List(Top, Middley, Bottom))(Move.apply)
  lazy val allSequences = allMoves.permutations.toSeq
}

object YCoordinates extends Enumeration {
  type Coordinate = Value
  val Top, Bottom, Middley = Value
}
object XCoordinates extends Enumeration {
  type Coordinate = Value
  val Left, Middlex, Right = Value
}

case class Move(x: XCoordinates.Value, y: YCoordinates.Value)
object Move {
  /** @return a Move object from a pair of coordinates */
  implicit def pairToMove(xy: (XCoordinates.Value, YCoordinates.Value)): Move = Move(xy._1, xy._2)
}

object Players {
  sealed trait Player
  object Player1 extends Player
  object Player2 extends Player
}