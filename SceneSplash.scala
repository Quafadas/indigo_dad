package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import scala.collection.View.Empty
import indigo.shared.materials.Material.ImageEffects
import indigo.shared.events.PointerEvent.PointerDown
import game.GameAssets.buttonSplashAssets
import game.GameAssets.buttonParamsAssets
import game.HelloIndigo.GetScaleFactor

object SceneSplash extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  var kount2 = 3
/*  
  var oldPos = Point(0,0)
  var oldSize = Size(99,99)
*/

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = SplashSceneViewModel

  val name: SceneName = SceneName("Splash")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.splashScene,
      (m, vm) => m.copy(splashScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  def subSystems = Set(SSSplash("Splash"))

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = { case _ =>
    if kount2 > 0 then
      println("@@@ SceneSplash-updateModel")
      kount2 = kount2 - 1
    end if
    Outcome(model)
  }
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case ViewportResize(gameViewPort) =>
      println("@@@ ViewportResize bounds:size" + gameViewPort.bounds + ":" + gameViewPort.size)
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

    val width = viewModel.viewPortWidth
    val height = viewModel.viewPortHeight

    val dSF = GetScaleFactor(width, height, GameAssets.SplashSceneDimensions)
    val textSplash = TextBox("Splash Scene V27 : " + dSF, 1000, 40)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(30))
      .moveTo(30, 0)

    //val bootData = context.frameContext.startUpData.flicFlacBootData
    //val width = bootData.pixelWidth
    //val height = bootData.pixelHeight

    Outcome {
      val layerBg = (GameAssets.splashBg)

      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black)))
        |+| SceneUpdateFragment(Layer(layerBg.scaleBy(dSF, dSF)))
        |+| SceneUpdateFragment(GameAssets.cornerLayers(GameAssets.SplashSceneDimensions, dSF, RGBA.Yellow)) // Splash Scene is 1920x1080
        |+| SceneUpdateFragment(textSplash.scaleBy(dSF,dSF))
        |+| SceneUpdateFragment(viewModel.rulesButton.draw)
        |+| SceneUpdateFragment(viewModel.playButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }
  end present
end SceneSplash

final case class SplashSceneViewModel(
//  splashButton: Button,
    viewPortWidth: Int,
    viewPortHeight: Int,
    rulesButton: Button,
    playButton: Button,
    resultsButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[SplashSceneViewModel] =
    for
//      bn1 <- splashButton.updateFrom(pointers)
      bn2 <- rulesButton.updateFromPointers(pointers)
      bn3 <- playButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    yield this.copy( /*splashButton = bn1,*/ rulesButton = bn2, playButton = bn3, resultsButton = bn4)

  def changeButtonBoundaries( ssvm : SplashSceneViewModel, gvp : GameViewport ) : SplashSceneViewModel =

    val dSF = GetScaleFactor(gvp.width, gvp.height, GameAssets.SplashSceneDimensions)
    println("@@@ dSF:"+dSF)

    val x1 = (50*dSF).toInt    // FIXME the following four values 50,50,240,80 need to #defines
    val y1 = (50*dSF).toInt
    val w1 = (240*dSF).toInt
    val h1 = (80*dSF).toInt
    val r1 = Rectangle(x1,y1,w1,h1)

    val newRulesButton =       
      Button(
        buttonAssets = GameAssets.buttonRulesAssets(dSF),
        bounds = r1,
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent)

    val x2 = (50*dSF).toInt    // FIXME the following four values 50,50,240,80 need to #defines
    val y2 = (150*dSF).toInt
    val w2 = (240*dSF).toInt
    val h2 = (80*dSF).toInt
    val r2 = Rectangle(x2,y2,w2,h2)

    val newPlayButton = 
      Button(
        buttonAssets = GameAssets.buttonPlayAssets(dSF),
        bounds = r2,
        depth = Depth(6)
      ).withUpActions(ButtonPlayEvent)

    val x3 = (50*dSF).toInt    // FIXME the following four values 50,50,240,80 need to #defines
    val y3 = (250*dSF).toInt
    val w3 = (240*dSF).toInt
    val h3 = (80*dSF).toInt
    val r3 = Rectangle(x3,y3,w3,h3)

    val newResultsButton = 
      Button(
        buttonAssets = GameAssets.buttonResultsAssets(dSF),
        bounds = r3,
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)


    this.copy(  viewPortWidth = gvp.width, 
                viewPortHeight = gvp.height, 
                rulesButton = newRulesButton,
                playButton = newPlayButton,
                resultsButton = newResultsButton
                )

end SplashSceneViewModel

object SplashSceneViewModel:

  val initial: SplashSceneViewModel =

    SplashSceneViewModel(
      viewPortWidth = 1920,
      viewPortHeight = 1080,

      /*
      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 20, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),
       */

      Button(
        buttonAssets = GameAssets.buttonRulesAssets(1.0),
        bounds = Rectangle(50, 50, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),
      Button(
        buttonAssets = GameAssets.buttonPlayAssets(1.0),
        bounds = Rectangle(50, 150, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonPlayEvent),
      Button(
        buttonAssets = GameAssets.buttonResultsAssets(1.0),
        bounds = Rectangle(50, 250, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )
end SplashSceneViewModel
