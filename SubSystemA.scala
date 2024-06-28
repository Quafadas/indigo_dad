package game

import indigo.*

final case class SSA(initialMessage: String) extends SubSystem[Model] { // I added [GameModel] here 
  type EventType = GlobalEvent
  type SubSystemModel = String
  type ReferenceData = Unit

  val id: SubSystemId = SubSystemId("SubSystemA")

  val eventFilter: GlobalEvent => Option[EventType] =
    _ =>None

  // Extra line here, as mandated by indigo's SubSystem.scala. Yet it is not in the examples!!!
  def reference(model: Model): Unit = ()  
  
  def initialModel: Outcome[String] = Outcome(initialMessage)

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message : String
  ): EventType => Outcome[String] =
    _ => Outcome(message)

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message : String
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
}
