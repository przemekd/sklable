package shared

import org.scalatest._
import flatspec._
import matchers._

class TileBagSpec extends AnyFlatSpec with should.Matchers {

  "Chars of tile bag" should "be properly sorted" in {
    val spanisTileBag: TileBag = TileBag.getTileBagByLang(SupportedLanguage.ES)
    spanisTileBag.getChars should be(List("A", "B", "C", "CH", "D", "E", "F", "G", "H", "I", "J", "L", "LL", "M", "N", "Ñ", "O", "P", "Q", "R", "RR", "S", "T", "U", "V", "X", "Y", "Z"))
  }

}
