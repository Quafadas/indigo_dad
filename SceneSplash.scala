package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import scala.collection.View.Empty
import indigo.shared.materials.Material.ImageEffects
import indigo.shared.events.PointerEvent.PointerDown

object SceneSplash extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = SplashSceneViewModel

  val name: SceneName = SceneName("Splash")
  var k0 = 0
  var k1 = 0
  var k2 = 0
  var k3 = 0
  var k4 = 0
  var k5 = 0

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
    case e: PointerEvent.PointerDown => 
      val mouseDown =  MouseEvent.MouseDown( e.position.x, e.position.y )
      println("@@@ PointerEventDown:" + e)
      k0 += 1
      k2 += 1
      Outcome(model).addGlobalEvents(mouseDown)

    case e: MouseEvent.MouseDown =>
      println("@@@ MouseEventDown:" + e)
      k1 += 1
      k2 += 1
      Outcome(model)
    
    case e: PointerEvent.PointerUp => 
      val mouseUp =  MouseEvent.MouseUp( e.position.x, e.position.y )
      println("@@@ PointerEventUp:" + e)
      k3 += 1
      k5 += 1
      Outcome(model).addGlobalEvents(mouseUp)

    case e: MouseEvent.MouseUp =>
      println("@@@ MouseEventUp:" + e)
      k4 += 1
      k5 += 1
      Outcome(model)

    case _ => 
      Outcome(model)
  }
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

    val textSplash = TextBox("Splash Scene V14 " +k0+":"+k1+":"+k2+":"+k3+":"+k4+":"+k5, 400, 40)
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
    
