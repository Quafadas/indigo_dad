package game

import scala.scalajs.js
import indigo.*
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import scala.collection.mutable.Queue
import io.circe.syntax.*
import io.circe.parser.decode
import cats.effect.kernel.Ref
import cats.effect.unsafe.ref.Reference

sealed trait WebRtcEvent extends GlobalEvent

object WebRtcEvent:
  case object MakePeerEntity extends WebRtcEvent // 10
  case class CreatedPeerEntity(peer: Peer) extends WebRtcEvent // 20
  case class Connect(s: String) extends WebRtcEvent with NetworkReceiveEvent // 30
  case class IncomingPeerConnection(conn: DataConnection) extends WebRtcEvent with NetworkReceiveEvent // 40
  case class PeerCreatedConnection(conn: DataConnection) extends WebRtcEvent // 50
  case object ConnectionOpen extends WebRtcEvent with NetworkReceiveEvent // 60
  case class SendData(ffgm: FlicFlacGameModel) extends WebRtcEvent with NetworkReceiveEvent // 70
  case class ReceivedData(data: js.Object) extends WebRtcEvent with NetworkReceiveEvent // 80
  case class Close() extends WebRtcEvent with NetworkReceiveEvent // 90

end WebRtcEvent
final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None
  var latestUpdate: Option[FlicFlacGameUpdate.Info] = None
  val eventQueue: Queue[WebRtcEvent] = Queue.empty[WebRtcEvent]

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemPeerJS")

  val eventFilter: GlobalEvent => Option[GlobalEvent] = {
    case e: GlobalEvent => Some(e)
    case null           => None
    }
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = flicFlacGameModel

  def initialModel: Outcome[Unit] =
    scribe.debug("@@@ SubSystemPeerJS initialModel")
    Outcome(())
  end initialModel

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] = {
    case WebRtcEvent.MakePeerEntity =>
      scribe.debug("@@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using " + context.reference.ourName)
      val localPeer = Peer(id = context.reference.ourName)

      localPeer.on(
        "open",
        (_: Any) =>
          scribe.debug("@@@-11 localPeer.on open")
          if (context.reference.ourName.compare(context.reference.oppoName) < 0) then 
            // we are the connection initiator so attempt to make the request
            eventQueue.enqueue(WebRtcEvent.Connect(context.reference.oppoName))
          end if
      )

      localPeer.on(
        "connection",
        (c: DataConnection) =>
          scribe.debug("@@@-12 localPeer.connection to " + c.label)

          // optionally, we can reject connection if c.label != context.reference.oppoName ...
          // ... this is the scenario where an unknown peer has connected to us

          eventQueue.enqueue(WebRtcEvent.IncomingPeerConnection(c))
      )
      localPeer.on(
        "disconnected",
        (_: Any) =>
          scribe.debug("@@@-13 localPeer.on disconnected")
      )

      localPeer.on(
        "close",
        (_: Any) =>
          scribe.debug("@@@-14 localPeer.on close")
      )

      localPeer.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@-19 LocalPeer.on error " + js.JSON.stringify(e))
      )
      eventQueue.enqueue(WebRtcEvent.CreatedPeerEntity(localPeer))
      Outcome(())

    case WebRtcEvent.CreatedPeerEntity(p) =>
      scribe.debug("@@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity")
      peer = Some(p)
      Outcome(())

    case WebRtcEvent.Connect(s) =>
      val ourname = peer.get.id
      scribe.debug("@@@-30 SubSystemPeerJS WebRtcEvent.Connect: " + ourname + " ->  " + s)

      val connection = peer match
        case Some(p) =>
          val options = js.Dynamic.literal()
          options.label = ourname
          val conn = p.connect(s, options)
          conn.on(
            "open",
            (_: Any) =>
              scribe.debug("@@@-31 Connect.on open")
              eventQueue.enqueue(WebRtcEvent.ConnectionOpen)
          )
          conn.on(
            "close",
            (c: DataConnection) =>
              scribe.debug("@@@-32 Connect.on close")
          )
          conn.on(
            "error",
            (e: js.Object) =>
              scribe.error("@@@-33 Connect.on error" + js.JSON.stringify(e))
          )

          conn

        case None =>
          scribe.fatal("@@@-39 Connect None ... Attempting to connect without a peer")
          val nullDataConnection = new DataConnection()
          nullDataConnection // likely to cause exception

      eventQueue.enqueue(WebRtcEvent.PeerCreatedConnection(connection))
      Outcome(())

    case WebRtcEvent.IncomingPeerConnection(c) =>
      scribe.debug("@@@-40 SubSystemPeerJS WebRtcEvent.IncomingPeerConnection")
      c.on(
        "data",
        (data: js.Object) =>
          scribe.debug("@@@-41 ConnectionOpen.on data ")
          val str = js.JSON.stringify(data)
          val ffgm = decodeRxJsonObject(data, 48) // 48 is the error number
          latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm))
      )

      c.on(
        "close",
        (c: DataConnection) =>
          scribe.debug("@@@-42 IncomingPeerConnection.on closed ")
      )
      c.on(
        "error",
        (e: js.Object) =>
          scribe.error("@@@-49 IncomingPeerConnection.on error " + js.JSON.stringify(e))
      )
      eventQueue.enqueue(WebRtcEvent.PeerCreatedConnection(c))
      Outcome(())

    case WebRtcEvent.PeerCreatedConnection(connLocal: DataConnection) =>
      scribe.debug("@@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection")
      conn = Some(connLocal)
      Outcome(())

    case WebRtcEvent.ConnectionOpen =>
      scribe.debug("@@@-60 SubSystemPeerJS WebRtcEvent.ConnectionOpen")
      conn.foreach { c =>
        c.on(
          "data",
          (data: js.Object) =>
            val ffgm = decodeRxJsonObject(data, 68) // 68 is the error number
            latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm))
        )

        c.on(
          "close",
          (c: DataConnection) =>
            scribe.debug("@@@-62 ConnectionOpen.on close ")
        )
        c.on(
          "error",
          (e: js.Object) =>
            scribe.error("@@@-69 ConnectionOpen.on error " + js.JSON.stringify(e))
        )
      }
      Outcome(())

    case WebRtcEvent.SendData(ffgm) =>
      scribe.debug("@@@-70 SubSystemPeerJS WebRtcEvent.SendData")
      val toSendNoSpaces = ffgm.asJson.noSpaces
      val toSendJson = js.JSON.parse(toSendNoSpaces)

      conn.foreach { c =>
        scribe.debug("@@@-71 SendData " + c.peer + "->" + c.label)
        c.send(toSendJson)
      }
      Outcome(())

    case WebRtcEvent.ReceivedData(data: js.Object) =>
      scribe.debug("@@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData")
      conn.foreach { c =>
        scribe.debug("@@@-81 ReceiveData " + c.label + "->" + c.peer)
        val ffgm = decodeRxJsonObject(data, 88) // 88 is the error number
        latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm))
      }
      Outcome(())

    case WebRtcEvent.Close() =>
      scribe.debug("@@@-90 SubSystemPeerJS WebRtcEvent.Close")
      conn.foreach(_.close())
      Outcome(())

    case _ =>
      val bEvents = !eventQueue.isEmpty
      (bEvents, latestUpdate) match
        case (true, Some(update)) => // ........................ Both Event(s) and Game Update Info
          val events = eventQueue.dequeueAll(_ => true)
          latestUpdate = None
          Outcome(()).addGlobalEvents(events*).addGlobalEvents(update)
        case (true, None) => // ................................ Just Event(s)
          val events = eventQueue.dequeueAll(_ => true)
          Outcome(()).addGlobalEvents(events*)
        case (false, Some(update)) => // ....................... Just Game Update Info
          latestUpdate = None
          Outcome(()).addGlobalEvents(update)
        case (false, None) => // ............................... Neither, idling
          Outcome(())
            
  }
  end update


  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =


    val boxX = 260
    val boxY = 136
    val boxW = 1000
    val boxH = 500


    val textError = TextBox("This is an error warning" , boxW-16, boxH-16)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(boxX+8, boxY+8)

    Outcome(
      SceneUpdateFragment.empty
      |+| SceneUpdateFragment(Shape.Box(Rectangle(boxX, boxY, boxW, boxH), Fill.Color(RGBA.Red)))
      |+| SceneUpdateFragment(Shape.Box(Rectangle(boxX+4, boxY+4, boxW-8, boxH-8), Fill.Color(RGBA.White)))
      |+| SceneUpdateFragment(textError)
      
    )
  end present

  def decodeRxJsonObject(data: js.Object, errNo: Int): FlicFlacGameModel =
    val str = js.JSON.stringify(data)
    val ffgm = decode[FlicFlacGameModel](str)
      .fold(
        e =>
          scribe.error("@@@-" + errNo + " Error decoding data")
          throw new Exception("Error " + errNo + "decoding data")
          ,
          identity
      )
    ffgm
  end decodeRxJsonObject

end SSGame



