package game

import indigo.*
import indigo.scenes.*

object SceneB extends Scene[StartUpData, Model, ViewModel] :
  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName = SceneName("SceneNameB")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  // Nothing to do
  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters = EventFilters.Permissive

  val subSystems = Set(SSB("Test2"))

  def updateModel(
    context: SceneContext[StartUpData], 
    model: SceneModel 
    ): GlobalEvent => Outcome[SceneModel] = 
      _ => Outcome(model)

  def updateViewModel(
    context: SceneContext[StartUpData], 
    model: SceneModel, 
    viewModel: SceneViewModel
    ): GlobalEvent => Outcome[SceneViewModel] =
      _ => Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.
    // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.
  def present(context: SceneContext[StartUpData], 
      model: SceneModel,
      viewModel: SceneViewModel):
         Outcome[SceneUpdateFragment] = {
    val events: Batch[GlobalEvent] =
      if (context.inputState.mouse.wasMouseClickedWithin(Rectangle(0, 0, 550, 400))) Batch(SceneEvent.JumpTo(SceneB.name))
      else Batch.empty

    val textA = TextBox("Hello Scene A", 20, 30 )
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))

    Outcome(
      SceneUpdateFragment(textA)
    ).addGlobalEvents(events)
  }

final case class MessageB(value: String)
