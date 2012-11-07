package tictactoe

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.{Group, Grouped}
import org.scalacheck._
import org.specs2.matcher.Matcher
import org.scalacheck.Prop.forAll
import Coordinates.{Top, Bottom, Left, Middle, Right}

class TicTacToeSpec extends Specification with Boards { def is =

  spec(tdd)^p^
  "WITH SCALACHECK"^p^
  spec(pdd)

  def spec(gs: Grouped) =
   "board description" ^ boardDescription(gs)^ endp^
   "winner"            ^ winner(gs)^ endp^
   "moves"             ^ moves(gs)

  def boardDescription(gs: Grouped) = { import gs._

    "a board has a 3 vertical lines"      ! g1.e1^
      "a board has a 3 horizontal lines"  ! g1.e2^
      "a board has a 2 diagonal lines"    ! g1.e3^
      "a board has a 8 lines to consider" ! g1.e4^
      "a line can be full"                ! g1.e5^
      "a line can be not full"            ! g1.e6^
      "a board is full when all lines are full" ! g1.e7^
      end
  }

  def winner(gs: Grouped) = { import gs._
   "an empty board has is not winning"            ! g2.e1^
   "a line can be winning"                        ! g2.e2^
   "a board is winning if one line is winning"    ! g2.e3^
   "a board is over if it is complete or winning" ! g2.e4^
   end
  }

  def moves(gs: Grouped) = { import gs._
    "a player can make a move on an empty board" ! g3.e1^
    end
  }

 object tdd extends Grouped {
   "board" - new g1 {
     e1 := Board.empty.verticalLines must have size(3)
     e2 := Board.empty.horizontalLines must have size(3)
     e3 := Board.empty.diagonalLines must have size(2)
     e4 := Board.empty.lines must have size(8)
     e5 := Line("x", "o", "x") must beFull
     e6 := Line("x", "o") must not(beFull)
     e7 := Board.complete must beComplete
   }

   "winner" - new g2 {
     e1 := Board() must not(win)
     e2 := Line("x", "x", "x") must beWinning
     e3 := Board(Line("x", "x", "x"), Line(), Line()) must win
     e4 := Board(Line("x", "x", "x"), Line("x", "x", "x"), Line("x", "x", "x")) must beOver
   }

   "moves" - new g3 {
     e1 := Board.empty.move((Top, Left)) must not(beComplete)
   }
 }

 object pdd extends Grouped {
   "board" - new g1 {
     e1 := prop { board: Board => board.verticalLines must have size(3) }
     e2 := prop { board: Board => board.horizontalLines must have size(3) }
     e3 := prop { board: Board => board.diagonalLines must have size(2) }
     e4 := prop { board: Board => board.lines must have size(8) }
     e5 := forAll(fullLines) { line: Line   => line must beFull }
     e6 := forAll(incompleteLines) { line: Line   => line must not(beFull) }
     e7 := forAll(completeBoards) { board: Board   => board must beComplete }
   }

   "winner" - new g2 {
     e1 := forAll(emptyBoards) { board: Board => board must not(win) }
     e2 := forAll(winningLines) { line: Line   => line must beWinning }
     e3 := forAll(winningBoards) { board: Board => board must win }
     e3 := forAll(overBoards) { board: Board => board must beOver }
   }
 }

}

trait Boards extends ScalaCheck { this: Specification =>

  implicit val arbitraryBoards: Arbitrary[Board] = Arbitrary { boards }
  implicit val arbitrarylines: Arbitrary[Line] = Arbitrary { lines }

  def boards            = Gen.oneOf(emptyBoards, completeBoards)
  def emptyBoards       = Gen.value(Board.empty)
  def overBoards        = Gen.oneOf(completeBoards, winningBoards)
  def completeBoards    = Gen.value(Board.complete)
  def winningBoards     = Gen.listOfN(8, winningLines).map(lines => Board(lines: _*))

  def lines             = Gen.oneOf(winningLines, fullLines)
  def winningLines      = Gen.oneOf("x", "o").map(p => Line(p, p, p))
  def fullLines         = Gen.listOfN(3, Gen.oneOf("x", "o")).map(l => Line(l:_*))
  def incompleteLines   = Gen.listOfN(2, Gen.oneOf("x", "o")).map(l => Line(l:_*))

  def beWinning: Matcher[Line] = (l: Line) => (l.isWinning, "the line is not winning "+l)
  def win: Matcher[Board] = (b: Board) => (b.isWinning, "the board is not winning "+b)

  def beFull: Matcher[Line] = (l: Line) => (l.isFull, "the line is not full "+l)
  def beComplete: Matcher[Board] = (b: Board) => (b.isComplete, "the board is not complete "+b)
  def beOver: Matcher[Board] = win or beComplete
}

object Coordinates extends Enumeration {
  type Coordinate = Value
  val Top, Bottom, Left, Middle, Right = Value
}

case class Line(moves: String*) {
  def isWinning = (moves.distinct.size == 1) && isFull
  def isFull    = moves.size == 3
}
case class Board(lines: Line*) {

  def move(xy: (Coordinates.Value, Coordinates.Value)) = this
  def verticalLines = Seq.fill(3)("")
  def horizontalLines = Seq.fill(3)("")
  def diagonalLines = Seq.fill(2)("")

  def isWinning = lines.exists(_.isWinning)
  def isComplete = lines.forall(_.isFull)

}

object Board {
  def empty = Board(Seq.fill(8)(Line()):_*)
  def complete = Board(Seq.fill(8)(Line("x","x","x")):_*)
}