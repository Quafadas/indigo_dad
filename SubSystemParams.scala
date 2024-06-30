package game

import indigo.*

final case class SSParams(initialMessage: String) extends SubSystem[Model]:
  type EventType = GlobalEvent
  type SubSystemModel = String
  type ReferenceData = Unit

  val id: SubSystemId = SubSystemId("SubSystemParams")

  val eventFilter: GlobalEvent => Option[EventType] =
    _ => None

  def reference(model: Model): Unit = ()

  def initialModel: Outcome[String] = Outcome(initialMessage)

  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): EventType => Outcome[String] =
    _ => Outcome(message)

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: String
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
end SSParams
