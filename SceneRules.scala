package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneRules extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

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
      Outcome(model)
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case ViewportResize(gameViewPort) =>
      println("@@@ SceneRules ViewportResize bounds:size" + gameViewPort.bounds + ":" + gameViewPort.size)
      Outcome(viewModel.changeButtonBoundaries(viewModel, gameViewPort))

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

    val width = viewModel.viewPortWidth
    val height = viewModel.viewPortHeight
    scribe.info("@@@ DANGER SCALE FACTOR !")
    val scaleFactor = 1.0 // FlicFlacGame().GetScaleFactor(width, height, GameAssets.SplashSceneDimensions)
    val sFactor = ((10*scaleFactor).toInt).toString()

    Outcome {
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(Layer(layerBg.scaleBy(scaleFactor, scaleFactor)))
        |+| SceneUpdateFragment(GameAssets.cornerLayers(GameAssets.RulesSceneDimensions, scaleFactor, RGBA.Black))
        |+| SceneUpdateFragment(Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Black)))
        |+| SceneUpdateFragment(TextBox(sFactor,50,20).withColor(RGBA.Yellow).withFontSize(Pixels(20)).moveTo(0,0))
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
    for
      bn1 <- splashButton.updateFromPointers(pointers)
//      bn2 <- rulesButton.update(pointers)
      bn3 <- playButton.updateFromPointers(pointers)
//      bn4 <- resultsButton.update(pointers)
    yield this.copy(splashButton = bn1, /*rulesButton = bn2,*/ playButton = bn3 /*, resultsButton = bn4*/ )

  def changeButtonBoundaries(ssvm: RulesSceneViewModel, gvp: GameViewport): RulesSceneViewModel =

    scribe.info("scale factor danger")
    val dSF = 1.0 // HelloIndigo.GetScaleFactor(gvp.width, gvp.height, GameAssets.RulesSceneDimensions)
    println("@@@ dSF:" + dSF)

    val x1 = (20 * dSF).toInt // FIXME the following four values 50,50,240,80 need to #defines
    val y1 = (20 * dSF).toInt
    val w1 = (240 * dSF).toInt
    val h1 = (80 * dSF).toInt
    val r1 = Rectangle(x1, y1, w1, h1)

    val newSplashButton =
      Button(
        buttonAssets = GameAssets.buttonSplashAssets(dSF),
        bounds = r1,
        depth = Depth(6)
      ).withUpActions(ButtonSplashEvent)

    val x2 = (20 * dSF).toInt // FIXME the following four values 50,50,150,80 need to #defines
    val y2 = (120 * dSF).toInt
    val w2 = (240 * dSF).toInt
    val h2 = (80 * dSF).toInt
    val r2 = Rectangle(x2, y2, w2, h2)

    val newPlayButton =
      Button(
        buttonAssets = GameAssets.buttonPlayAssets(dSF),
        bounds = r2,
        depth = Depth(6)
      ).withUpActions(ButtonPlayEvent)

    this.copy(
      viewPortWidth = gvp.width,
      viewPortHeight = gvp.height,
      splashButton = newSplashButton,
      playButton = newPlayButton
    )
  end changeButtonBoundaries

end RulesSceneViewModel

object RulesSceneViewModel:

  val initial: RulesSceneViewModel =
    RulesSceneViewModel(
      viewPortWidth = 1700, // FIXME ... should be getting the current screen size somehow
      viewPortHeight = 1250, // FIXME ... should be getting the current screen size somehow
      Button(
        buttonAssets = GameAssets.buttonSplashAssets(1.0),
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
        buttonAssets = GameAssets.buttonPlayAssets(1.0),
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
