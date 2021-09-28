package server

object AckingReceiver {
  case object Ack

  case object StreamInitialized
  case object StreamCompleted
  final case class StreamFailure(ex: Throwable)
}
