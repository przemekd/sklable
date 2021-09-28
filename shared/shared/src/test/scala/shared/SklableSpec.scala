package shared

import org.scalatest._
import flatspec._
import matchers._
import shared.Board.{Position, Rows}

class SklableSpec extends AnyFlatSpec with should.Matchers {

  "A point calculator" should "properly calculate points" in {
    val move: Sklable.Move = List((RegularTile(1, "A", 1), Position(Rows.A, 1)))
    Sklable.calculatePoints(List.empty, move) should be(0)
  }

  "A moves validator" should "properly validate moves" in {
    val move: Sklable.Move = List((RegularTile(1, "A", 1), Position(Rows.A, 1)))
    Sklable.isValidMove(List.empty, move) should be(false)
  }

  "Is tileNextTo" should "work" in {
    val move: Sklable.Move = List((RegularTile(1, "A", 1), Position(Rows.A, 1)))
    val move2: Sklable.Move = List((RegularTile(1, "A", 1), Position(Rows.A, 2)))
    Sklable.isTileNextTo(List(move), move2.head) should be(true)
  }

}
