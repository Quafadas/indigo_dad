package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneResults extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = ResultsSceneViewModel

  val name: SceneName = SceneName("Results")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.resultsScene,
      (m, vm) => m.copy(resultsScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSResults("Results"))

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =
    case _ =>
      Outcome(model)
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case _ =>
      Outcome(viewModel)
  end updateViewModel

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val textResults = TextBox("Results Scene", 400, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    Outcome {
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(textResults)
        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| SceneUpdateFragment(viewModel.rulesButton.draw)
        |+| SceneUpdateFragment(viewModel.gameButton.draw)
//        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }
  end present
end SceneResults

final case class ResultsSceneViewModel(
    splashButton: Button,
    rulesButton: Button,
    gameButton: Button
//  resultsButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[ResultsSceneViewModel] =
    for
      bn1 <- splashButton.updateFromPointers(pointers)
      bn2 <- rulesButton.updateFromPointers(pointers)
      bn3 <- gameButton.updateFromPointers(pointers)
//      bn4 <- resultsButton.updateFromPointers(pointers)
    yield this.copy(splashButton = bn1, rulesButton = bn2, gameButton = bn3 /*, resultsButton = bn4*/ )
end ResultsSceneViewModel

object ResultsSceneViewModel:

  val initial: ResultsSceneViewModel =
    ResultsSceneViewModel(
      Button(
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 60, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonSplashEvent),
      Button(
        buttonAssets = GameAssets.buttonRulesAssets,
        bounds = Rectangle(20, 160, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),
      Button(
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(20, 260, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent)
      /*
      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
       */
    )
end ResultsSceneViewModel
