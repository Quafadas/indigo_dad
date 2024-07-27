package game

import indigo.*

final case class SSGame(initialMessage: String) extends SubSystem[FlicFlacGameModel]:
  type EventType = GlobalEvent
  type SubSystemModel = String
  type ReferenceData = Unit

  val id: SubSystemId = SubSystemId("SubSystemGame")

  val eventFilter: GlobalEvent => Option[EventType] = {
    case e: GlobalEvent => 
      if (e==SubSysGameUpdate) then Some(e)
      else None
    case null => None
  }

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(flicFlacGameModel: FlicFlacGameModel): Unit = ()

  def initialModel: Outcome[String] = Outcome(initialMessage)

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): EventType => Outcome[String] = {

    case SubSysGameUpdate =>  Outcome(message)
    case _ => Outcome(message)
  }

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSGame
