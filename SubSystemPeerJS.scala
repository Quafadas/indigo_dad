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

/*
INITIATOR                                                            | RESPONDER
@@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using FIRSTNAME    | @@@-10 SubSystemPeerJS WebRtcEvent.MakePeerEntity using SECONDNAME
@@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity                 | @@@-20 SubSystemPeerJS WebRtcEvent.CreatedPeerEntity
@@@-11 localPeer.on open                                             | @@@-11 localPeer.on open
                                                                     |
@@@-30 SubSystemPeerJS WebRtcEvent.Connect: FIRSTNAME -> SECONDNAME  |
@@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection             |
                                                                     | @@@-12 localPeer.connection to FIRSTNAME
                                                                     | @@@-40 SubSystemPeerJS WebRtcEvent.IncomingPeerConnection
@@@-31 Connect.on open                                               |
                                                                     | @@@-50 SubSystemPeerJS WebRtcEvent.PeerCreatedConnection
@@@-60 SubSystemPeerJS WebRtcEvent.ConnectionOpen                    |
@@@-70 SubSystemPeerJS WebRtcEvent.SendData                          |
@@@-71 SendData ALPHA                                                |
                                                                     | @@@-41 IncomingPeerConnection.on data ALPHA
                                                                     | @@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData
                                                                     | @@@-81 ReceiveData ALPHA
                                                                     | @@@-70 SubSystemPeerJS WebRtcEvent.SendData
                                                                     | @@@-71 SendData bravo
@@@-61 ConnectionOpen.on data bravo                                  |
@@@-80 SubSystemPeerJS WebRtcEvent.ReceiveData                       |
@@@-81 ReceiveData bravo                                             |
@@@-90 SubSystemPeerJS WebRtcEvent.Close                             |
@@@-32 Connect.on close                                              |
@@@-62 ConnectionOpen.on close                                       |
                                                                     | @@@-42 IncomingPeerConnection.on closed
*/

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

val timerDivisor = 100  // makes this timer operate in 10ths of a second ... 0 means disabled, +20 = 2 seconds
var timerT1: Long = 0

final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  var peer: Option[Peer] = None
  var conn: Option[DataConnection] = None
  var latestUpdate: Option[FlicFlacGameUpdate.Info] = None
  val eventQueue: Queue[WebRtcEvent] = Queue.empty[WebRtcEvent]

  // panelMsg controls the appearance of the Error/Pause/Results panel. It can be triggered in two different ways 1)&2)
  // and it s cleared in one way 3)
  // 1) direct manipulation of the var panelMsg & set bIssueFreezeNow=true from the callbacks in this file
  // 2) indirectly on receipt of the message Frozen.PanelContent(T,msg) where T is one of P_ERROR, P_PAUSE, P_RESULTS
  // 3) receipt of message Frozen.PanelContent(P_INVISIBLE,"") ... which occurs when you click outside the board
 

  var panelMsg: (PanelType, String) = (PanelType.P_INVISIBLE, "")

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
  ): EventType => Outcome[Unit] =
    e => 
      try {
        e match
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
                setGameState(GameState.START_CON2 ,context.reference)
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
                panelMsg = (PanelType.P_ERROR, js.JSON.stringify(e) + " HINT: Wait for \"" + context.reference.oppoName + "\" to start")  // signalling ERROR
                timerT1 = timerStart(100) // 10 seconds
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
            timerT1 = timerStop()

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
                    timerT1 = timerStop() // successful connection so stop timer
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
                    panelMsg = (PanelType.P_ERROR, js.JSON.stringify(e))  // signalling ERROR
                )

                conn

              case None =>
                scribe.fatal("@@@-39 Connect None ... Attempting to connect without a peer")
                panelMsg = (PanelType.P_ERROR, "Connecting without a local peer in place")  // signalling ERROR                
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
                panelMsg = (PanelType.P_ERROR, js.JSON.stringify(e))  // signalling ERROR
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
                  panelMsg = (PanelType.P_ERROR, js.JSON.stringify(e))  // signalling ERROR
              )
            }
            Outcome(())

          case WebRtcEvent.SendData(ffgm) =>
            scribe.debug("@@@-70 SubSystemPeerJS WebRtcEvent.SendData")

            conn.foreach { c =>
              if (c.open) then
                scribe.debug("@@@-71 SendData " + c.peer + "->" + c.label)
                val toSendNoSpaces = ffgm.asJson.noSpaces
                val toSendJson = js.JSON.parse(toSendNoSpaces)
                c.send(toSendJson)
              else
                scribe.debug("@@@-72 SendData blocked as connection not open yet")
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

          case Freeze.PanelContent(typeOfPanel,messageToDisplay) =>
            panelMsg = (typeOfPanel, messageToDisplay)
            Outcome(())


          case _ =>
            if (timerExpired(timerT1)) then 
                timerT1 = timerStop()
                if (context.reference.ourName.compare(context.reference.oppoName) < 0) then 
                  // we are the connection initiator and timerT1 has expired so attempt to the connection request again
                  eventQueue.enqueue(WebRtcEvent.Connect(context.reference.oppoName))                
                end if
            end if 

            val bEvents = !eventQueue.isEmpty
            (bEvents, latestUpdate) match
              case (true, Some(update)) => // ......................... Both Event(s) and Game Update Info
                val events = eventQueue.dequeueAll(_ => true)
                latestUpdate = None
                Outcome(()).addGlobalEvents(events*).addGlobalEvents(update)
              case (true, None) => // ................................. Just Event(s)
                val events = eventQueue.dequeueAll(_ => true)
                Outcome(()).addGlobalEvents(events*)
              case (false, Some(update)) => // ........................ Just Game Update Info
                latestUpdate = None
                Outcome(()).addGlobalEvents(update)
              case (false, None) => // ................................ Idling
                Outcome(())
        } 
        catch {
          case e : PeerJsException => 
            val errorMsg = e.getMessage()
            scribe.error("@@@ SubSystemPeerJS update PeerJsCustomException handler " + errorMsg)
            panelMsg = (PanelType.P_ERROR, errorMsg)
            Outcome(())
          case e : Throwable =>
            val errorMsg = e.getMessage()
            scribe.error("@@@ SubSystemPeerJS update ThrowableException handler " + errorMsg)
            panelMsg = (PanelType.P_ERROR, errorMsg)
            Outcome(())
        }
  
  end update


  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =

    panelMsg  match {
      case (PanelType.P_ERROR, msg) =>
        displayErrorPanel(msg)
      case (PanelType.P_PAUSE, msg) =>
        displayPausePanel(msg)
      case (PanelType.P_RESULTS, msg) =>
        displayResultsPanel(msg)
      case _ => // including P_INVISIBLE
        Outcome(SceneUpdateFragment.empty)        
    }
  end present

  def displayErrorPanel(msg:String) : Outcome[SceneUpdateFragment] =
    val boxX = 260
    val boxY = 136
    val boxW = (16 + (12 * (msg.length()))).max(1000)
    val boxH = 180

    val textError1 = TextBox("*** FlicFlac ERROR ***" , boxW-16, boxH-16)
      .alignCenter.bold.withColor(RGBA.Red).withFontSize(Pixels(60)).moveTo(boxX+8, boxY+8)

    val textError2 = TextBox(msg , boxW-16, boxH-16)
      .withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(boxX+8, boxY+100)

    val textError3 = TextBox("(Click on any part of the white border to dismiss this notification)", boxW-16, boxH-16)
      .withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(boxX+8, boxY+140)

    Outcome(
      SceneUpdateFragment.empty
      |+| SceneUpdateFragment(Shape.Box(Rectangle(boxX, boxY, boxW, boxH), Fill.Color(RGBA.Red)))
      |+| SceneUpdateFragment(Shape.Box(Rectangle(boxX+4, boxY+4, boxW-8, boxH-8), Fill.Color(RGBA.Cyan)))
      |+| SceneUpdateFragment(textError1)
      |+| SceneUpdateFragment(textError2)
      |+| SceneUpdateFragment(textError3)
    )

  end displayErrorPanel

  def displayPausePanel(msg:String) : Outcome[SceneUpdateFragment] = 
    Outcome(SceneUpdateFragment.empty) // FIXME not implemented yet    
  end displayPausePanel
  
  def displayResultsPanel(msg:String) : Outcome[SceneUpdateFragment] = 
    Outcome(SceneUpdateFragment.empty) // FIXME not implemented yet    
  end displayResultsPanel



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

  def timerStart(expiryTime: Long) : Long =// expiryTime is in 10ths of a second
    val timer = (System.currentTimeMillis() / timerDivisor) + expiryTime
    timer
  end timerStart

  def timerStop() : Long =
    val timer = 0
    timer
  end timerStop

  def timerExpired(timer: Long) : Boolean =
    val currentTime = (System.currentTimeMillis() / timerDivisor)
    val expired = (timer > 0) && (currentTime >= timer)
    expired
  end timerExpired

  def setGameState(gs: GameState, ffgm: FlicFlacGameModel) : Unit = 
    //
    latestUpdate match
      case Some(update) =>
        // peerJs has already modified the model, so we must update the modification
        val existingModel = update.ffgm
        latestUpdate = Some(FlicFlacGameUpdate.Info(existingModel.copy(gameState = gs)))

      case None =>
        // peerJs has not modified the model so we need a new model to report
        latestUpdate = Some(FlicFlacGameUpdate.Info(ffgm.copy(gameState = gs)))
  end setGameState
  
end SSGame

class PeerJsException(cause: String) extends Exception(cause)


