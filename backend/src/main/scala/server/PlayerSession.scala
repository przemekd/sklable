package server

import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.scaladsl.{ BroadcastHub, Keep, Sink, Source }
import akka.http.scaladsl.model.ws.Message

object PlayerSession {
  val AckMessage = AckingReceiver.Ack

  // sent from stream to actor to indicate start, end or failure of stream:
  val InitMessage       = AckingReceiver.StreamInitialized
  val OnCompleteMessage = AckingReceiver.StreamCompleted
  val onErrorMessage    = (ex: Throwable) => AckingReceiver.StreamFailure(ex)

  def getSinkAndSource(player: shared.Player)(implicit actorSystem: ActorSystem) = {

    // https://github.com/johanandren/chat-with-akka-http-websockets/tree/akka-2.6
    val (wsActor, source) = Source
      .actorRef[Message](10, akka.stream.OverflowStrategy.dropTail)
      .toMat(BroadcastHub.sink[Message])(Keep.both)
      .run()

    val receiver = actorSystem.actorOf(Props(classOf[PlayerActor], player, wsActor))
    val sink     = Sink.actorRefWithBackpressure(receiver, InitMessage, AckMessage, OnCompleteMessage, onErrorMessage)

    (sink, source, receiver)
  }
}
