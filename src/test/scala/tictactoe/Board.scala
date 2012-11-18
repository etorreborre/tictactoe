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

  /** transform a board by using one of it's natural symmetries */
  def transform(symmetry: Symmetry) = Board.create(moves.map(_.transform(symmetry)))

  override def toString =
    Move.allMoves.sliding(3, step = 3).toSeq.transpose.map(_.map(show).mkString("|", "|", "|")).mkString("\n", "\n", "\n")

  private def movesForPlayer(i: Int) = moves.zipWithIndex.filter(_._2 % 2 == i).map(_._1)
  private def firstPlayerMoves       = movesForPlayer(0)
  private def secondPlayerMoves      = movesForPlayer(1)

  private def firstPlayerWins       = Board.isWinning(firstPlayerMoves)
  private def secondPlayerWins      = Board.isWinning(secondPlayerMoves)

  def show(move: Move) =
    moves.zipWithIndex.collect { case (m, i) if m == move => if (i % 2 == 0) "o" else "x" }.headOption.getOrElse(" ")


}

/**
 * The Symmetry case class encapsulates a coordinate transformation to be applied to all moves of a Board
 */
case class Symmetry(changeX: XCoordinates.Value => XCoordinates.Value, changeY: YCoordinates.Value => YCoordinates.Value) {
  def apply(x: XCoordinates.Value, y: YCoordinates.Value) =
    (changeX(x), changeY(y))
}

object Symmetry {

  /**
   * list of all applicable symmetries.
   *
   * It is built by computing all the combinations where there is a symmetry with the x coordinates and/or the
   * y coordinates
   */
  lazy val symmetries: Seq[Symmetry] =
    ^(xsymmetries, ysymmetries)((x, y) => Symmetry(x, y))

  private lazy val xsymmetries = symmetriesOf(XCoordinates.values.toSeq)
  private lazy val ysymmetries = symmetriesOf(YCoordinates.values.toSeq)

  /**
   * @return 2 symmetries:
   *         the identity and the permutation of the first and last element
   *         This would need to be generalized for a n x n board
   */
  private def symmetriesOf[T](values: Seq[T]) =
    List(values, values.last +: values.drop(1).dropRight(1) :+ values.head).map { permuted: Seq[T] =>
      (t: T) => values.toIndexedSeq(permuted.indexOf(t))
    }
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
  def verticalWins: Seq[Seq[Move]]   = Seq(verticalWin1, verticalWin2, verticalWin3)
  /** @return winning diagonal lines */
  def diagonalWins: Seq[Seq[Move]]   = Seq(diagonalWin1, diagonalWin2)

  /** @return the first winning diagonal line */
  lazy val diagonalWin1: Seq[Move]   =
    Seq(Move(Left, Top), Move(Middlex, Middley), Move(Right, Bottom))

  /** @return the second winning diagonal line */
  lazy val diagonalWin2: Seq[Move]   =
    Seq(Move(Left, Bottom), Move(Middlex, Middley), Move(Right, Top))

  lazy val verticalWin1 = YCoordinates.values.toSeq.map(y => Move(Left,    y))
  lazy val verticalWin2 = YCoordinates.values.toSeq.map(y => Move(Middlex, y))
  lazy val verticalWin3 = YCoordinates.values.toSeq.map(y => Move(Right,   y))

  lazy val minimumWinLines = Seq(Board.verticalWin1, Board.verticalWin2, Board.diagonalWin1)

  /** @return all possible winning moves */
  lazy val winningMoves: Seq[Seq[Move]]   = horizontalWins ++ verticalWins ++ diagonalWins

  def isWinning(playerMoves: Seq[Move]): Boolean = Board.winningMoves.exists(_.forall(playerMoves.contains))

  lazy val allSequences = Move.allMoves.permutations.toSeq
}

object YCoordinates extends Enumeration {
  type Coordinate = Value
  val Top, Middley, Bottom = Value
}
object XCoordinates extends Enumeration {
  type Coordinate = Value
  val Left, Middlex, Right = Value
}

case class Move(x: XCoordinates.Value, y: YCoordinates.Value) {
  def transform(symmetry: Symmetry): Move = symmetry(x, y)
}
object Move {
  /** @return a Move object from a pair of coordinates */
  implicit def pairToMove(xy: (XCoordinates.Value, YCoordinates.Value)): Move = Move(xy._1, xy._2)

  lazy val allMoves = ^(XCoordinates.values.toList, YCoordinates.values.toList)(Move.apply)
}

object Players {
  sealed trait Player
  object Player1 extends Player {
    override def toString = "player1"
  }
  object Player2 extends Player {
    override def toString = "player2"
  }
}