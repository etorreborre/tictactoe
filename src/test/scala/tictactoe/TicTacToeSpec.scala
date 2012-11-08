package tictactoe

import org.specs2._
import specification._
import collection.Iterablex._
import matcher.Matcher
import org.scalacheck._
import org.scalacheck.Prop.forAll
import XCoordinates.{Left, Middlex, Right}
import YCoordinates.{Top, Bottom, Middley}
import scalaz.Scalaz._

class TicTacToeSpec extends Specification with Boards { def is =

  spec(tdd)          ^
                     p^
  "WITH SCALACHECK"  ^p^
  spec(pdd)

  def spec(gs: Grouped) = playerMoves(gs)

  def playerMoves(gs: Grouped) = { import gs._
    "player moves"^
      "it is possible to make a move on an empty board"                ! g1.e1^
      "it is not possible to make a move that has already been played" ! g1.e2^
      "it is not possible to make more than 9 moves"                   ! g1.e3^
      "a player is winning if his moves occupy a line"                 ! g1.e4^
    end
  }

 object tdd extends Grouped {
   "moves" - new g1 {
     e1 := { Board.empty.move(Left, Top) must not(beFull) }
     e2 := { Board.empty.move(Left, Top).canMove(Left, Top) must beFalse }
     e3 := { Board.empty.move(allMoves:_*).canMove(Left, Top) must beFalse }
     e4 := { Board.empty.move((Left, Top), (Right, Bottom), (Middlex, Top), (Left, Bottom), (Right, Top)).isWinning must beTrue }
   }
 }

 object pdd extends Grouped {
   "moves" - new g1 {
     e1 := forAll(emptyBoards, moves)             { (b: Board, m: Move) => b.move(m) must not(beFull) }
     e2 := forAll(emptyBoards, movesSeq(), moves) { (b: Board, mvs: Seq[Move], m: Move) =>
       b.move(mvs:_*).canMove(m) must beFalse.when(mvs.contains(m))
     }
     e3 := forAll(emptyBoards, movesSeq(9, 9), moves) { (b: Board, mvs: Seq[Move], m: Move) =>
       b.move(mvs:_*).canMove(m) must beFalse
     }
     e4 := forAll(winningBoards) { b: Board =>
       b.isWinning must beTrue
     }
   }
 }

}

trait Boards extends ScalaCheck { this: Specification =>
  def beFull: Matcher[Board] = (b: Board) => (b.isFull, b+" is not full")

  implicit val arbitraryBoards: Arbitrary[Board] = Arbitrary(boards)
  implicit val arbitraryMoves: Arbitrary[Move]   = Arbitrary(moves)

  def boards        = Gen.oneOf(emptyBoards, winningBoards)
  def emptyBoards   = Gen.value(Board.empty)

  /**
   * winning boards are created by interspersing a winning sequence of moves (a "line") with an arbitrary sequence.
   * This is not all winning boards though
   */
  def winningBoards = for {
    player1Moves  <- winningSeq
    player2Moves  <- movesSeq(3, 3)
  } yield Board(player1Moves.zip(player2Moves).flatMap { case (p1, p2) => Seq(p1, p2) }.dropRight(1):_*)

  /** sequence of 3 moves which make a winning line */
  def winningSeq                           = Gen.choose(0, 7).map(Board.winningMoves)
  def allMoves                             = ^(List(Left, Middlex, Right), List(Top, Middley, Bottom))(Move.apply)
  def moves                                = Gen.oneOf(allMoves)
  def movesSeq(min: Int = 0, max: Int = 9) = Gen.choose(min, max).map(n => allMoves.take(n).scramble)
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
  implicit def pairToMove(xy: (XCoordinates.Value, YCoordinates.Value)): Move = Move(xy._1, xy._2)
}

case class Board(moves: Move*) {
  def isFull = moves.size == 9

  def move(x: XCoordinates.Value, y: YCoordinates.Value): Board = move(Move(x, y))
  def move(mvs: Move*): Board                                   = mvs.foldLeft(this)((b, m) => Board((b.moves :+ m):_*))

  def canMove(x: XCoordinates.Value, y: YCoordinates.Value): Boolean = canMove(Move(x, y))
  def canMove(m: Move): Boolean                                      = !moves.contains(m)

  private def movesForPlayer(i: Int) = moves.zipWithIndex.filter(_._2 % 2 == i).map(_._1)
  private def firstPlayerMoves       = movesForPlayer(0)
  private def secondPlayerMoves      = movesForPlayer(1)

  def isWinning(playerMoves: Seq[Move]): Boolean = Board.winningMoves.exists(_.forall(playerMoves.contains))
  def isWinning: Boolean = Seq(firstPlayerMoves, secondPlayerMoves).exists(isWinning)
}

object Board {
  def empty = Board()

  def horizontalWins: Seq[Seq[Move]] = YCoordinates.values.toSeq.map(y => XCoordinates.values.toSeq.map(Move(_,y)))
  def verticalWins: Seq[Seq[Move]]   = XCoordinates.values.toSeq.map(x => YCoordinates.values.toSeq.map(Move(x,_)))
  def diagonalWins: Seq[Seq[Move]]   = Seq(Seq(Move(Left, Top), Move(Middlex, Middley), Move(Right, Bottom)),
    Seq(Move(Left, Bottom), Move(Middlex, Middley), Move(Right, Top)))

  def winningMoves: Seq[Seq[Move]]   = horizontalWins ++ verticalWins ++ diagonalWins

}