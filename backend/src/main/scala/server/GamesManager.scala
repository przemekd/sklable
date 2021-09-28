package server

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object GamesManager {
  def apply(): Behavior[SklableManagerMessage.ManagerCommand] = {
    println("Creating Sklable Games Manager")
    Behaviors.setup(context => manager(Map.empty))
  }

  private def manager(
      games: Map[String, akka.actor.typed.ActorRef[server.SklableGameCommands.Command]]
  ): Behavior[SklableManagerMessage.ManagerCommand] = {
    import SklableManagerMessage._
    println(s"Number of rooms: ${games.size}")

    Behaviors
    .receive[ManagerCommand] { (context, message) =>
      message match {
        case AddUserToRoom(player, roomId) =>
          context.log.debug(s"We want to add $player")
          games.get(roomId) match {
            case Some(room) =>
              player ! SklableManagerMessage.CreatedRoom(room)
              Behaviors.same
            case None =>
              val game = context.spawn(SklableGame(roomId), s"scrable-game-$roomId")
              context.watchWith(game, SklableManagerMessage.TerminatedRoom(roomId))
              player ! SklableManagerMessage.CreatedRoom(game)
              manager(games + (roomId -> game))
          }
        case CreatedRoom(_) =>
          Behaviors.same
        case TerminatedRoom(roomId) => 
          context.log.info(s"Removing room: $roomId")
          manager(games.filterNot { case (id, ref) => id == roomId})
      }
    }
  }
}

object SklableManagerMessage {
  sealed trait ManagerCommand
  final case class CreatedRoom(game: akka.actor.typed.ActorRef[server.SklableGameCommands.Command])
      extends ManagerCommand
  final case class AddUserToRoom(player: akka.actor.ActorRef, roomId: String) extends ManagerCommand
  final case class TerminatedRoom(roomId: String) extends ManagerCommand
}
