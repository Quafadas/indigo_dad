package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneRules extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  var kount2 = 5

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = RulesSceneViewModel

  val name: SceneName = SceneName("Rules")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.rulesScene,
      (m, vm) => m.copy(rulesScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSRules("Rules"))

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =
    case _ =>
      if kount2 > 0 then
        println("@@@ SceneRules-updateModel")
        kount2 = kount2 - 1
      end if
      Outcome(model)
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      println("@@@ SceneRules - FrameTick TP1")
      val v = viewModel.update(context.mouse, context.frameContext.inputState.pointers)
      println("@@@ SceneRules - FrameTick TP2")
      v

    case ViewportResize(gameViewPort) =>
      println("@@@ ViewportResize w:h=" + gameViewPort.width + ":" + gameViewPort.height)
      Outcome(viewModel.copy(
        viewPortWidth = gameViewPort.width,
        viewPortHeight = gameViewPort.height))

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

    val layerBg = (GameAssets.rulesBg)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    //val width = bootData.pixelWidth
    //val height = bootData.pixelHeight
    val width = viewModel.viewPortWidth
    val height = viewModel.viewPortWidth
       
    Outcome {
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(Layer(layerBg))  // FIXME scaling ???
        |+| SceneUpdateFragment(viewModel.splashButton.draw)
//        |+| SceneUpdateFragment(viewModel.rulesButton.draw)
        |+| SceneUpdateFragment(viewModel.playButton.draw)
//        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }
  end present
end SceneRules

final case class RulesSceneViewModel(
    viewPortWidth: Int,
    viewPortHeight: Int,
    splashButton: Button,
//  rulesButton: Button,
    playButton: Button
//  resultsButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[RulesSceneViewModel] =
    println("@@@ update TPX")
    for
      bn1 <- splashButton.updateFromPointers(pointers)
//      bn2 <- rulesButton.update(pointers)
      bn3 <- playButton.updateFromPointers(pointers)
//      bn4 <- resultsButton.update(pointers)
    yield this.copy(splashButton = bn1, /*rulesButton = bn2,*/ playButton = bn3 /*, resultsButton = bn4*/ )
end RulesSceneViewModel

object RulesSceneViewModel:

  val initial: RulesSceneViewModel =
    RulesSceneViewModel(
      viewPortWidth = 1700,     // FIXME ... should be getting the current screen size somehow
      viewPortHeight = 1250,    // FIXME ... should be getting the current screen size somehow
      Button(
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 20, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonSplashEvent),
      /*
      Button (
        buttonAssets = GameAssets.buttonRulesAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),
       */
      Button(
        buttonAssets = GameAssets.buttonPlayAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonPlayEvent)
      /*
      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
       */
    )
end RulesSceneViewModel
