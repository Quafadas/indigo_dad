package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneGame extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  val name: SceneName = SceneName("Game")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSGame("Game"))

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
      viewModel.update(context.mouse)

    case _ => 
      Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val textGame = TextBox("Game Scene", 400, 40)
      .withColor(RGBA.Indigo)
      .withFontSize(Pixels(30))
      .moveTo(300, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    Outcome {
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(textGame)
        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| SceneUpdateFragment(viewModel.paramsButton.draw)
        |+| SceneUpdateFragment(viewModel.gameButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }

final case class GameSceneViewModel(
  splashButton: Button,
  paramsButton: Button,
  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse): Outcome[GameSceneViewModel] =
    for {
      bn1 <- splashButton.update(mouse)
      bn2 <- paramsButton.update(mouse)
      bn3 <- gameButton.update(mouse)
      bn4 <- resultsButton.update(mouse)
    } yield this.copy( splashButton = bn1, paramsButton = bn2, gameButton = bn3, resultsButton = bn4)

object GameSceneViewModel:

  val initial: GameSceneViewModel = 
    GameSceneViewModel(
      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 20, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),

      Button (
        buttonAssets = GameAssets.buttonParamsAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonParamsEvent),

      Button (
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent),

      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )
    
