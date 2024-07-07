package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

object SceneSplash extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

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

    val textSplash = TextBox("Splash Scene 5", 400, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    // SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
    Outcome {
      val layerBg = (GameAssets.splashBg)
      val dWidth: Double = width
      val dHeight: Double = height
      val dsfx : Double = dWidth/1920
      val dsfy : Double = dHeight/1080
      val sf : Double = if ( width * 1080 > height *1920 ) then dsfy else dsfx

      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black)))
        |+| SceneUpdateFragment(Layer(layerBg.scaleBy(sf, sf)))
        |+| SceneUpdateFragment(textSplash)
//        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| SceneUpdateFragment(viewModel.paramsButton.draw)
        |+| SceneUpdateFragment(viewModel.gameButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }

final case class SplashSceneViewModel(
//  splashButton: Button,
  paramsButton: Button,
  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse): Outcome[SplashSceneViewModel] =
    for {
//      bn1 <- splashButton.update(mouse)
      bn2 <- paramsButton.update(mouse)
      bn3 <- gameButton.update(mouse)
      bn4 <- resultsButton.update(mouse)
    } yield this.copy( /*splashButton = bn1,*/ paramsButton = bn2, gameButton = bn3, resultsButton = bn4)

object SplashSceneViewModel:

  val initial: SplashSceneViewModel = 
    SplashSceneViewModel(
/*      
      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 20, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),
*/
      Button (
        buttonAssets = GameAssets.buttonParamsAssets,
        bounds = Rectangle(50, 50, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonParamsEvent),

      Button (
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(50, 150, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent),

      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(50, 250, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )
    
