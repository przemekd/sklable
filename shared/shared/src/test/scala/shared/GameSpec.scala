package shared

import org.scalatest._
import flatspec._
import matchers._

class GameSpec extends AnyFlatSpec with should.Matchers {

  "A nextplayer" should "properly return player id when no moves" in {
    val game: Game =
      Game(
        InProgress,
        Seq(Player("1", "1"), Player("2", "2"), Player("3", "3")),
        Seq(Player("3", "3"), Player("2", "2"), Player("1", "1")),
        Seq(),
        Seq(),
        Seq(),
        TileBag.emptyBag
      )
    game.playerToMove.get should be("3")
  }

  "A nextplayer" should "properly return player id" in {
    val game: Game =
      Game(
        InProgress,
        Seq(Player("1", "1"), Player("2", "2"), Player("3", "3")),
        Seq(Player("1", "1"), Player("2", "2"), Player("3", "3")),
        Seq(),
        Seq(),
        Seq(("1", shared.Pass, 0)),
        TileBag.emptyBag
      )
    game.playerToMove.get should be("2")
  }

}
