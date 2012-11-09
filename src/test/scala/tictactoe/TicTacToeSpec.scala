package tictactoe

import org.specs2._
import specification._
import org.scalacheck.Prop.forAll
import XCoordinates.{Left, Middlex, Right}
import YCoordinates.{Top, Bottom, Middley}
import Players._
import Board._

/**
 * ====================
 * This whole exercise was inspired by: http://www.natpryce.com/articles/000795.html
 * (Exploring Test-Driven Development with QuickCheck)
 * ====================
 *
 * This specification gives a few rules for scoring a game of tic-tac-toe.
 *
 * Each rule is actually implemented twice: once using a traditional TDD approach providing concrete examples and
 * twice using a PDD (Property-Driven-Development) approach using ScalaCheck properties
 *
 */
class TicTacToeSpec extends Specification with Boards { def is =

  playerMoves(tdd)   ^
  "WITH SCALACHECK"  ^p^
  playerMoves(pdd)

  /** list of rules for making moves on a board */
  def playerMoves(gs: Grouped) = { import gs._
    "player moves"^
      "it is possible to make a move on an empty board"                ! g1.e1^
      "it is not possible to make a move that has already been played" ! g1.e2^
      "it is not possible to make more than 9 moves"                   ! g1.e3^
      "a player is winning if his moves occupy a line"                 ! g1.e4^
      "player1 is winning if his moves occupy a line"                  ! g1.e5^
      "player2 is winning if his moves occupy a line"                  ! g1.e6^
    endp
  }

 /** implementation of the rules using TDD */
 object tdd extends Grouped {
   new g1 {

     e1 := { Board.empty.move(Left, Top) must not(beFull) }
     e2 := { Board.empty.move(Left, Top).canMove(Left, Top) must beFalse }
     e3 := { Board.empty.move(allMoves:_*).canMove(Left, Top) must beFalse }
     e4 := { Board.empty.move((Left, Top), (Right, Bottom), (Middlex, Top), (Left, Bottom), (Right, Top)).isWinning }
     e5 := { Board.empty.move((Left, Top), (Right, Bottom),
                              (Middlex, Top), (Left, Bottom),
                              (Right, Top)).winner ==== Some(Player1) }
     e6 := { Board.empty.move((Middlex, Middley), (Left, Top),
                              (Right, Bottom), (Middlex, Top),
                              (Left, Bottom), (Right, Top)).winner ==== Some(Player2) }
   }
 }

  /** implementation of the rules using PDD */
  object pdd extends Grouped {
    new g1 {

     e1 := forAll(emptyBoards, moves) { (b: Board, m: Move) => b.move(m) must not(beFull) }

     e2 := forAll(emptyBoards, moveSeqs(), moves) { (b: Board, mvs: Seq[Move], m: Move) =>
       b.move(mvs:_*).canMove(m) must beFalse.when(mvs.contains(m))
     }

     e3 := forAll(emptyBoards, moveSeqs(9, 9), moves) { (b: Board, mvs: Seq[Move], m: Move) =>
       b.move(mvs:_*).canMove(m) must beFalse
     }

     e4 := forAll(winningBoards)    (_.isWinning)
     e5 := forAll(player1WinsBoards)(_.winner ==== Some(Player1))
     e6 := forAll(player2WinsBoards)(_.winner ==== Some(Player2))
   }
 }

}
