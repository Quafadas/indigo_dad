package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import scala.collection.View.Empty
import indigo.shared.materials.Material.ImageEffects
import indigo.shared.events.PointerEvent.PointerDown


object SceneSplash extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  var kount2 = 300

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
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {
    case _ => 
      if (kount2 > 0)
        println("@@@ SceneSplash-updateModel")
        kount2 = kount2 - 1    
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

    case _ => 
      Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val textSplash = TextBox("Splash Scene V25")
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(20))
      .moveTo(30, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    Outcome {
      val layerBg = (GameAssets.splashBg)
      val dWidth: Double = width
      val dHeight: Double = height
      val dsfx : Double = dWidth/1920
      val dsfy : Double = dHeight/1080
      //val sf : Double = if ( width * 1080 > height *1920 ) then dsfy else dsfx
      val sf = 1.0  //FIXME no scaling for the moment and perhaps this is a permanent decision !!!    

      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black)))
        |+| SceneUpdateFragment(Layer(layerBg.scaleBy(sf, sf)))
        |+| SceneUpdateFragment(GameAssets.cornerLayers(1920,1080,sf,RGBA.Yellow))  // Splash Scene is 1920x1080
        |+| SceneUpdateFragment(textSplash)
//        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| SceneUpdateFragment(viewModel.rulesButton.draw)
        |+| SceneUpdateFragment(viewModel.gameButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
    }

final case class SplashSceneViewModel(
//  splashButton: Button,
  rulesButton: Button,
  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[SplashSceneViewModel] =
    for {
//      bn1 <- splashButton.updateFrom(pointers)
      bn2 <- rulesButton.updateFromPointers(pointers)
      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    } yield this.copy( /*splashButton = bn1,*/ rulesButton = bn2, gameButton = bn3, resultsButton = bn4)


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
        buttonAssets = GameAssets.buttonRulesAssets,
        bounds = Rectangle(50, 50, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),

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

