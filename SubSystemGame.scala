package game

import scala.scalajs.js
import indigo.*
import java.util.Timer
import indigo.platform.networking.Network
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import io.circe.syntax.*
import io.circe.parser.decode

case class FlicFlacNewInformationEvent(ffgm: FlicFlacGameModel) extends SubSystemEvent

sealed trait WebRtcEvent extends GlobalEvent

object WebRtcEvent:
  case object MakePeerConnection extends WebRtcEvent
  case class RecievedData(ffgm: FlicFlacGameModel) extends WebRtcEvent
  case class SendGameData(ffgm: FlicFlacGameModel) extends WebRtcEvent

end WebRtcEvent

final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None

  var latestUpdate: Option[FlicFlacGameModel] = None

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemGame")

  // val eventFilter: GlobalEvent => Option[EventType] = Some(_)
  // Some(e)
  // =>
  // case e: WebRtcEvent => Some(e)
  // case _              => None

  val eventFilter: GlobalEvent => Option[WebRtcEvent] = {
    case e: WebRtcEvent => Some(e)
    case _              => None
    }

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = flicFlacGameModel

  def initialModel: Outcome[Unit] =
    scribe.debug("@@@ SubSystemGame initialModel")

    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {
    case WebRtcEvent.SendGameData(ffgm) =>
      scribe.debug("@@@ SubSystemGame WebRtcEvent.SendGameData")
      val toSend = ffgm.asJson.noSpaces
      scribe.debug(s"@@@ $toSend")
      conn.foreach(_.send(js.JSON.parse(toSend)))
      Outcome(())

    case WebRtcEvent.MakePeerConnection =>
      scribe.debug("@@@ SubSystemGame WebRtcEvent.MakePeerConnection using ...")
      scribe.debug(s"@@@ data ${context.reference}")
      peer = Some(Peer(id = context.reference.ourName))
      peer.foreach(
        _.on(
          "connection",
          (c: DataConnection) =>
            val peerName = c.peer
            scribe.debug("@@@ SubSystemGame We made a connection with " + peerName)
            conn = Some(c)
            c.on(
              "data",
              (data: js.Object) =>
                val str = js.JSON.stringify(data)
                scribe.debug(s"@@@ SubSystemGame received data $data")

                val ffgm = decode[FlicFlacGameModel](str)
                  .fold(
                    e =>
                      scribe.error(s"Error decoding data: $e")
                      throw new Exception("Error decoding data")
                    ,
                    identity
                  )
                latestUpdate = Some(ffgm)
            )
        )
      )
      peer.foreach { p =>
        scribe.info(s"@@@ SubSystemGame Connecting to ${context.reference.oppoName}")
        val dataConn = p.connect(context.reference.oppoName)
        conn = Some(dataConn)
      }
      Outcome(())

/*
    case _ =>
      latestUpdate.fold {
        Outcome(())
      } { ffgm =>
        Outcome(()).addGlobalEvents(WebRtcEvent.RecievedData(ffgm))
      }
*/        
    case _ =>
      scribe.debug("@@@ SubSystemGame Default Handler")
      latestUpdate.fold {
        Outcome(())
      } { ffgm =>
        Outcome(()).addGlobalEvents(WebRtcEvent.RecievedData(ffgm))
      }

  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
