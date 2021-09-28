package services

import org.scalatest._
import flatspec._
import matchers._

class I18nServiceSpec extends AnyFlatSpec with should.Matchers {
  "I18nService" should "should discover supported languages" in {
    I18nService.supportedLanguages should be("en")
  }

}
