package server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import wvlet.log.LogSupport

import scala.util.{Failure, Success}

object SklableServer extends App with LogSupport {
  implicit val system = ActorSystem()
  import system.dispatcher

  val config = system.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")

  val service = new Webservice

  val binding = Http().newServerAt(interface, port).bind(service.route)

  binding.onComplete {
    case Success(binding) =>
      val localAddress = binding.localAddress
      info(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
    case Failure(e) =>
      error(s"Binding failed with ${e.getMessage}")
      system.terminate()
  }
}
