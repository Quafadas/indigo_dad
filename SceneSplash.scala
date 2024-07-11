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

    val textSplash = TextBox("Splash Scene V16 " +k0+":"+k1+":"+k2+":"+k3+":"+k4+":"+k5, 400, 40)
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
  def update(mouse: Mouse, pointers: Pointers): Outcome[SplashSceneViewModel] =
    for {
//      bn1 <- splashButton.update(mouse)
      bn2 <- paramsButton.updateFromPointers(pointers)
      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
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


/** This is a workaround to show a way to make buttons support simple pointer events. It is a simplified version of the
  * standard Button update function.
  */
extension (b: Button)
  def updateFromPointers(p: Pointers): Outcome[Button] =
    val inBounds = b.bounds.isPointWithin(p.position)

    val upEvents: Batch[GlobalEvent] =
      if inBounds && p.released then b.onUp()
      else Batch.empty

    val downEvents: Batch[GlobalEvent] =
      if inBounds && p.pressed then b.onDown()
      else Batch.empty

    val pointerEvents: Batch[GlobalEvent] =
      downEvents ++ upEvents

    b.state match
      // Stay in Down state
      case ButtonState.Down if inBounds && p.pressed =>
        Outcome(b).addGlobalEvents(b.onHoldDown() ++ pointerEvents)

      // Move to Down state
      case ButtonState.Up if inBounds && p.pressed =>
        Outcome(b.toDownState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Out of Down state
      case ButtonState.Down if !inBounds && (p.pressed || p.released) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      case ButtonState.Down if inBounds && p.released =>
        Outcome(b.toUpState).addGlobalEvents(pointerEvents)

      // Unaccounted for states.
      case _ =>
        Outcome(b).addGlobalEvents(pointerEvents)

/** This is a workaround to make up for Pointer not exposing any convenience methods.
  */
extension (p: Pointers)

  def pressed: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerDown => true
      case _                           => false
    }

  def released: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerUp => true
      case _                         => false
    }