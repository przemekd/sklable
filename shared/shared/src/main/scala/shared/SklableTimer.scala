package shared

final case class SklableTimer(millisecondsLeft: Int) extends AnyVal {
  def canContinue: Boolean = millisecondsLeft > 0

  override def toString: String =
    if (millisecondsLeft <= 0) "00:00"
    else s"${"%02d".format(millisecondsLeft / 60000)}:${"%02d".format((millisecondsLeft / 1000) % 60)}"
}
