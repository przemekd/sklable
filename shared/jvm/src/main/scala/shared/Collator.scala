package shared

import java.text
import java.util.Locale

object Collator {
  def getInstance(langCode: String) =
    Ordering.comparatorToOrdering(
      text.Collator.getInstance(new Locale(langCode.toLowerCase()))
    )
}
