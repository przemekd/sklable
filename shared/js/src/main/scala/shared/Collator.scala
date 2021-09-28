package shared

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal("Intl.Collator")
class Collator[T](langCode: String) extends js.Object {
  def compare(x: T, y: T): Integer = js.native
}

object Collator {
    def getInstance[T](langCode: String) = new Ordering[T] {
        private val collator = new Collator[T](langCode.toLowerCase())
        def compare(x: T, y: T): Int = collator.compare(x, y)
    }
}
