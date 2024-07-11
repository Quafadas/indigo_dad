package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.math.*

import org.scalajs.dom

// *******************************************
// Outstanding issues ...ViewModel.initialViewModel
// Browser refresh resets game
// Remove scale factor from Right Mouse Button
// Support Scenes
// Brown Area for Home & perhaps boarder ?
// Rework to ViewModelControl
// *******************************************

@JSExportTopLevel("IndigoGame")
object Game:
  def main(args: Array[String]): Unit =
    println("@@@ Object Game main Launch Start")
    HelloIndigo.launch(
      "indigo-container",
      Map[String, String](
        "width" -> dom.window.innerWidth.toString,
        "height" -> dom.window.innerHeight.toString
      )
    )
    println("@@@ Object Game main Launch Finish")
end Game

// gameSize can be one of 2,3,4,5,6 and is the number of major hexagons across one side where ...
// ... a major hexagon is ring of 6 hexagons, with a central 7th black hexagon
//val gameSize = 2 // <<<<<<<<<<<<<<<<<<<<<<<
//val boardBasePoint : Point = Point(400,50)  // where the (inisible) top left hand corner of the hex grid board is positioned

object HelloIndigo extends IndigoGame[FlicFlacBootData, FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  var kount1 = 3
  var kount2 = 3
  var kount3 = 3
  var kount4 = 3
  

  println("@@@ Object HelloIndigo Starts")

  val magnification = 1

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)


  val assets: Set[AssetType] =
    GameAssets.get()

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def setup(
      flicFlacBootData: FlicFlacBootData,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[FlicFlacStartupData]] =
    println("@@@ FlicFlacMain-setup()")
    val outCome = FlicFlacStartupData.initialise(flicFlacBootData)
    outCome

  def initialModel(flicFlacStartupData: FlicFlacStartupData): Outcome[FlicFlacGameModel] =
    println("@@@ FlicFlacMain-initialModel()")
    Outcome(FlicFlacGameModel())

  def initialScene(flicFlacBootData: FlicFlacBootData): Option[SceneName] =
    println("@@@ FlicFlacMain-initialScene()")
    Some(SceneSplash.name)
    //None

  // JP 30/06/24
  // To get the hexboard back as the main screen, comment out the populated "NonEmptyList" and ...
  // restore the line NonEmptyList(Scene.empty)

  def scenes(flicFlacBootData: FlicFlacBootData): NonEmptyList[Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]] =
    println("@@@ FlicFlacMain-scenes()")
    NonEmptyList(SceneSplash, SceneParams, SceneGame, SceneResults)

  def boot(flags: Map[String, String]): Outcome[BootResult[FlicFlacBootData,FlicFlacGameModel]] =
    println("@@@ FlicFlacMain-boot")
    println("@@@ BootFlags: " + flags)
    val width = flags("width").toInt
    val height = flags("height").toInt
    Outcome{
      val flicFlacBootData: FlicFlacBootData =
        FlicFlacBootData.create(width, height)
        //ViewConfig.default

      val config = 
        FlicFlacConfig.config
          .withViewport(flicFlacBootData.viewport)

      val assetPath: String = 
        flags.getOrElse("baseUrl","")

      BootResult(config, flicFlacBootData)
        .withAssets(assets)
    }

  def initialViewModel(flicFlacStartupData: FlicFlacStartupData, flicFlacGameModel: FlicFlacGameModel): Outcome[FlicFlacViewModel] =
    println("@@@ FlicFlacMain-initialViewModel()")
    val staticAssets = flicFlacStartupData.staticAssets
    Outcome(FlicFlacViewModel(
      staticAssets,
      SplashSceneViewModel.initial,
      ParamsSceneViewModel.initial,
      GameSceneViewModel.initial,
      ResultsSceneViewModel.initial
      )
    )

  def updateViewModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): GlobalEvent => Outcome[FlicFlacViewModel] =
    case FrameTick =>
      if (kount4 > 0)
        println("@@@ FlicFlacMain-updateViewModel FrameTick")
        kount4 = kount4 - 1
      Outcome(flicFlacViewModel)

    case ViewportResize(gameViewPort) =>
      val w = gameViewPort.width
      val h = gameViewPort.height
      //flicFlacViewModel.gameScene.
      println("@@@ FlicFlacMain-updateViewModel ViewportResize w:h " + w +":" +h)
      Outcome(flicFlacViewModel)

    case _ =>
      if (kount3 > 0)
        println("@@@ FlicFla cMain-updateViewModel _")
        kount3 = kount3 - 1
      Outcome(flicFlacViewModel)
  end updateViewModel

  def updateModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = 

    case ButtonSplashEvent =>
      println("@@@ Main-ButtonSplashEvent")
      Outcome(flicFlacGameModel).addGlobalEvents(SceneEvent.JumpTo(SceneSplash.name))

    case ButtonParamsEvent =>
      println("@@@ Main-ButtonParamsEvent")
      Outcome(flicFlacGameModel).addGlobalEvents(SceneEvent.JumpTo(SceneParams.name))

    case ButtonGameEvent =>
      println("@@@ Main-ButtonGameEvent")
      Outcome(flicFlacGameModel).addGlobalEvents(SceneEvent.JumpTo(SceneGame.name))

    case ButtonResultsEvent =>
      println("@@@ Main-ButtonResultsEvent")
      Outcome(flicFlacGameModel).addGlobalEvents(SceneEvent.JumpTo(SceneResults.name))

    case _ => 
      if (kount2 > 0)
        println("@@@ FlicFlacMain-updateModel")
        kount2 = kount2 - 1
      Outcome(flicFlacGameModel)

  def present(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): Outcome[SceneUpdateFragment] = Outcome {

    if (kount1 > 0)
      println("@@@ FlicFlacMain-present")
      kount1 = kount1 - 1
      
    SceneUpdateFragment.empty
  }

  println("@@@ Object HelloIndigo Finishes")

end HelloIndigo

// defining a view model that contains UI features for this scene
//final case class ViewModel(button1: Button, button2: Button)

//final case class Model(center: Point):
//  def update(timeDelta: Seconds): Model =
//    this.copy()
//end Model
//object Model:
//  def initial(center: Point): Model = Model(center)
//end Model

final case class FlicFlacGameModel()

//final case class ViewModel()
final case class FlicFlacViewModel(
    staticAssets: StaticAssets,
    splashScene: SplashSceneViewModel,
    paramsScene: ParamsSceneViewModel,
    gameScene: GameSceneViewModel,
    resultsScene: ResultsSceneViewModel
  )

case object ButtonSplashEvent extends GlobalEvent
case object ButtonParamsEvent extends GlobalEvent
case object ButtonGameEvent extends GlobalEvent
case object ButtonResultsEvent extends GlobalEvent




