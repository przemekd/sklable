package server

import org.scalatest._
import flatspec._
import matchers._
import shared.SupportedLanguage
import shared.RegularTile

class WordCheckerSpec extends AnyFlatSpec with should.Matchers {
  private def T(c: String)              = RegularTile(1, c, 1)
  private def toTilesList(word: String) = word.map(c => T(c.toString())).toList

  "A Polish word checker" should "properly classify words as valid" in {
    val result = WordChecker.wordsExists(
      SupportedLanguage.PL,
      List(
        toTilesList("WY"),
        toTilesList("JESTEŚCIE"),
        toTilesList("DOBRZY")
      )
    )
    result should be(true)
  }

  "A Spanish word checker" should "properly classify words as valid" in {
    val result = WordChecker.wordsExists(
      SupportedLanguage.ES,
      List(
        toTilesList("ESTAIS"),
        toTilesList("ERES"),
        toTilesList("ESTUPENDO"),
        List(T("E"), T("LL"), T("A"))
      )
    )
    result should be(true)
  }

  "A Spanish word checker" should "not accept words built with two tiles " +
    "(C, H), (L, L), (R, R) instead of one" in {
    val result = WordChecker.wordsExists(
      SupportedLanguage.ES,
      List(
        List(T("E"), T("L"), T("L"), T("A"))
      )
    )

    result should be(false)
  }

}
