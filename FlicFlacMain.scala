package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.math.*
import io.circe.parser.decode
import scribe.*
import scribe.format._


import org.scalajs.dom
import tyrian.TyrianSubSystem
import cats.effect.IO
import tyrian.TyrianIndigoBridge

// *******************************************
// Outstanding issues ...ViewModel.initialViewModel
// Browser refresh resets game
// Remove scale factor from Right Mouse Button
// Support Scenes
// Brown Area for Home & perhaps boarder ?
// Rework to ViewModelControl
// *******************************************

// gameSize can be one of 2,3,4,5,6 and is the number of major hexagons across one side where ...
// ... a major hexagon is ring of 6 hexagons, with a central 7th black hexagon
//val gameSize = 2 // <<<<<<<<<<<<<<<<<<<<<<<
//val boardBasePoint : Point = Point(400,50)  // where the (inisible) top left hand corner of the hex grid board is positioned

case class FlicFlacGame(
    tyrianSubSystem: TyrianSubSystem[IO, Int, FlicFlacGameModel]
) extends IndigoGame[FlicFlacBootData, FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  // scribe reporting levels: fatal,error,warn,info,debug,trace
  //val myFormatter: Formatter = formatter"[$threadName] $positionAbbreviated - $message$newLine"
  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .withMinimumLevel(Level.Debug)
    .replace()

  var kount1 = 3
  var kount2 = 3
  var kount3 = 3
  var kount4 = 3

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
    scribe.debug("@@@ FlicFlacMain-setup()")
    val outCome = FlicFlacStartupData.initialise(flicFlacBootData)
    outCome
  end setup

  def initialModel(flicFlacStartupData: FlicFlacStartupData): Outcome[FlicFlacGameModel] =
    scribe.debug("@@@ FlicFlacMain-initialModel()")

    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem("FlicFlac")) match

      case Right(model: FlicFlacGameModel) =>
        scribe.debug("@@@ Restored model")
        model
      case Left(_) =>
        scribe.debug("@@@ Created model")
        FlicFlacGameModel.creation(Point(0, 0))

    Outcome(cacheOrNew)
  end initialModel

  def initialScene(flicFlacBootData: FlicFlacBootData): Option[SceneName] =
    scribe.debug("@@@ FlicFlacMain-initialScene()")
    Some(SceneSplash.name)
  end initialScene

  // JP 30/06/24
  // To get the hexboard back as the main screen, comment out the populated "NonEmptyList" and ...
  // restore the line NonEmptyList(Scene.empty)

  def scenes(
      flicFlacBootData: FlicFlacBootData
  ): NonEmptyList[Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]] =
    scribe.debug("@@@ FlicFlacMain-scenes()")
    NonEmptyList(SceneSplash, SceneRules, SceneGame, SceneResults)
  end scenes

  def boot(flags: Map[String, String]): Outcome[BootResult[FlicFlacBootData, FlicFlacGameModel]] =
    scribe.debug("@@@ FlicFlacMain-boot")
    scribe.debug("@@@ BootFlags: " + flags)
    val width = flags("width").toInt
    val height = flags("height").toInt
    Outcome {
      val flicFlacBootData: FlicFlacBootData =
        FlicFlacBootData.create(width, height)
        // ViewConfig.default

      val config =
        FlicFlacConfig.config
          .withViewport(flicFlacBootData.viewport)

      val assetPath: String =
        flags.getOrElse("baseUrl", "")

      BootResult(config, flicFlacBootData)
        .withAssets(assets)
        .withSubSystems(tyrianSubSystem)
    }
  end boot

  def initialViewModel(
      flicFlacStartupData: FlicFlacStartupData,
      flicFlacGameModel: FlicFlacGameModel
  ): Outcome[FlicFlacViewModel] =
    scribe.debug("@@@ FlicFlacMain-initialViewModel()")
    val w = flicFlacStartupData.flicFlacBootData.gameViewPort.width
    val h = flicFlacStartupData.flicFlacBootData.gameViewPort.height
    val staticAssets = flicFlacStartupData.staticAssets
    Outcome(
      FlicFlacViewModel(
        staticAssets,
        SplashSceneViewModel.initial,
        RulesSceneViewModel.initial,
        GameSceneViewModel.initial,
        ResultsSceneViewModel.initial,
        ParamsSceneViewModel.initial,
        flicFlacStartupData.flicFlacBootData.gameViewPort,
      )
    )
  end initialViewModel

  def updateViewModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): GlobalEvent => Outcome[FlicFlacViewModel] =
    case FrameTick =>
      if kount4 > 0 then
        scribe.debug("@@@ FlicFlacMain-updateViewModel FrameTick")
        kount4 = kount4 - 1
      end if
      Outcome(flicFlacViewModel)

    case ViewportResize(gameViewPort) =>
      val w = gameViewPort.width
      val h = gameViewPort.height
      scribe.debug("@@@ FlicFlacMain-updateViewModel ViewportResize w:h " + w + ":" + h)
      // flicFlacGameModel.hexBoard3.calculateXpYp(1.0) // FIXME this needs to be immutable!!!
      Outcome(flicFlacViewModel.copy(theGameViewPort = gameViewPort))

    case ButtonSplashEvent =>
      scribe.debug("@@@ Main-ButtonSplashEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneSplash.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case ButtonRulesEvent =>
      scribe.debug("@@@ Main-ButtonRulesEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneRules.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case ButtonPlayEvent =>
      scribe.debug("@@@ Main-ButtonGameEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneGame.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case ButtonResultsEvent =>
      scribe.debug("@@@ Main-ButtonResultsEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneResults.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case _ =>
      if kount3 > 0 then
        scribe.debug("@@@ FlicFlac Main-updateViewModel _")
        kount3 = kount3 - 1
      end if
      Outcome(flicFlacViewModel)
  end updateViewModel

  def updateModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =

    case _ =>
      if kount2 > 0 then
        scribe.debug("@@@ FlicFlacMain-updateModel")
        kount2 = kount2 - 1
      end if
      Outcome(flicFlacGameModel)
  end updateModel

  def present(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): Outcome[SceneUpdateFragment] = Outcome {

    if kount1 > 0 then
      scribe.debug("@@@ FlicFlacMain-present")
      kount1 = kount1 - 1
    end if

    SceneUpdateFragment.empty
  }

  scribe.debug("@@@ FlicFlacMain class FlicFlacGame Finish")
end FlicFlacGame


def GetScaleFactor(viewWidth: Int, viewHeight: Int, sceneDimensions: Rectangle): Double =
  val dsfx: Double = viewWidth.toDouble / sceneDimensions.width
  val dsfy: Double = viewHeight.toDouble / sceneDimensions.height
  val dToCheck = if dsfx > dsfy then dsfy else dsfx

  val dSF =
    if dToCheck >= 1.0 then 1.0
    else if dToCheck >= 0.9 then 0.9
    else if dToCheck >= 0.8 then 0.8
    else if dToCheck >= 0.75 then 0.75
    else if dToCheck >= 0.67 then 0.67
    else if dToCheck >= 0.5 then 0.5
     else if dToCheck >= 0.33 then 0.33
    else 0.25
  dSF
end GetScaleFactor


//final case class ViewModel()
final case class FlicFlacViewModel(
    staticAssets: StaticAssets,
    splashScene: SplashSceneViewModel,
    rulesScene: RulesSceneViewModel,
    gameScene: GameSceneViewModel,
    resultsScene: ResultsSceneViewModel,
    paramsScene: ParamsSceneViewModel,
    theGameViewPort: GameViewport,
)

case object ButtonSplashEvent extends GlobalEvent
case object ButtonRulesEvent extends GlobalEvent
case object ButtonPlayEvent extends GlobalEvent
case object ButtonNewGameEvent extends GlobalEvent
case object ButtonResultsEvent extends GlobalEvent
case object ButtonParamsEvent extends GlobalEvent
case object ButtonPlusEvent extends GlobalEvent
case object ButtonMinusEvent extends GlobalEvent
case object ButtonTurnEvent extends GlobalEvent
case object SubSysGameUpdate extends GlobalEvent

