package game

import indigo.*
import io.github.quafadas.peerscalajs.DataConnection
import io.github.quafadas.peerscalajs.Peer

final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:
  type EventType = GlobalEvent
  type SubSystemModel = String
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemGame")

  var peer: Option[Peer] = None
  var dataConnection: Option[DataConnection] = None

  val eventFilter: GlobalEvent => Option[EventType] = {
    case e: GlobalEvent =>
      if e == SubSysGameUpdate then Some(e)
      else None
      end if
    case null => None
  }

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = 
    flicFlacGameModel

  def initialModel: Outcome[String] = 
    {
      scribe.debug("@@@ SubSystemGame initialModel")
      Outcome("initialModel")
    }

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): EventType => Outcome[String] = {

    case SubSysGameUpdate =>
      val (s1:String, s2:String) = FlicFlacPlayerParams.GetNames()
      scribe.debug("@@@ SubSystemGameUpdate with " + s1 + " playing " + s2)
      Outcome(message)

      peer match
        case Some(p) if p.id != s1 => peer = Some(Peer(id = s1))
        case Some(_) => ()
        case None => peer = Some(Peer(id = s1))
      

      Outcome(s1)

    case _                => Outcome(message)
  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame

/*---
// when p rxes a connection event ... this function fires
    val p = new Peer(id = id)
    p.on("connection", (data: DataConnection) => {

        data.on("data", (data: js.Object) => {
          println(s"Received data: ${js.JSON.stringify(data)}")
        })
        dataConnection.set(Some(data))  // sets our data connection variable to be that connection
        println(s"Received data connection from ${data.peer}")
    })

// send your opponents id
  val data = p.connect(inId) // p.connect(s2)
          data.on("data", (data: js.Object) => {
            println(s"Received data: ${js.JSON.stringify(data)}")
          })
          dataConnection.set(Some(data))   

// sending a message between 
  case (dataConn, msg) =>
    println(s"sending message: $msg")
    dataConn.foreach(_.send(msg))         
*/