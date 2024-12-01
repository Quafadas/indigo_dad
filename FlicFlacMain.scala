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
import scribe.format.*

import org.scalajs.dom
import tyrian.TyrianSubSystem
import cats.effect.IO
import tyrian.TyrianIndigoBridge

case class FlicFlacGame(
    tyrianSubSystem: TyrianSubSystem[IO, Int, FlicFlacGameModel]
) extends IndigoGame[FlicFlacBootData, FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  // scribe reporting levels: fatal,error,warn,info,debug,trace
  // val myFormatter: Formatter = formatter"[$threadName] $positionAbbreviated - $message$newLine"
  Logger.root
    .clearHandlers()
    .withHandler(formatter = Formatter.simple)
    .withMinimumLevel(Level.Debug)
    .replace()

  val magnification = 1

  val config: GameConfig =
    // The default from indigo was the following line ...
    // GameConfig.default.withMagnification(magnification)
    // ... but Simon discovered we need to use our own config in order to control viewport
    // properly and avoid "scaling & chopping issues" ... JP 26/08/24
    FlicFlacConfig.config

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
    val cachedParamsOrNew = FlicFlacPlayerParams.getParams(flicFlacStartupData)
    scribe.debug(s"@@@ PlayerParams: $cachedParamsOrNew")
    val newTurnTime = cachedParamsOrNew.playPams4_TurnTime
    val newCaptorsTime = cachedParamsOrNew.playPams5_CaptorsTime
    val newTT = TurnTimer(newTurnTime, newCaptorsTime)
    val cachedGameOrNew = FlicFlacGameModel.retrieve(flicFlacStartupData)
    val updatedGame = cachedGameOrNew.copy(turnTimer = newTT)
    Outcome(updatedGame).addGlobalEvents(WebRtcEvent.MakePeerEntity)
  end initialModel

  def initialScene(flicFlacBootData: FlicFlacBootData): Option[SceneName] =
    scribe.debug("@@@ FlicFlacMain-initialScene()")
    Some(SceneParams.name)
  end initialScene

  // JP 30/06/24
  // To get the hexboard back as the main screen, comment out the populated "NonEmptyList" and ...
  // restore the line NonEmptyList(Scene.empty)

  def scenes(
      flicFlacBootData: FlicFlacBootData
  ): NonEmptyList[Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]] =
    scribe.debug("@@@ FlicFlacMain-scenes()")
    NonEmptyList(SceneParams, SceneGame)
  end scenes

  def boot(flags: Map[String, String]): Outcome[BootResult[FlicFlacBootData, FlicFlacGameModel]] =
    scribe.debug("@@@ FlicFlacMain-boot")
    scribe.debug("@@@ BootFlags: " + flags)
    val width = flags("width").toInt
    val height = flags("height").toInt
    val name1: String = flags("name1")
    val name2: String = flags("name2")
    Outcome {
      val flicFlacBootData: FlicFlacBootData =
        FlicFlacBootData.create(width, height, name1, name2)
        // ViewConfig.default

      val config =
        FlicFlacConfig.config
          .withViewport(flicFlacBootData.viewport)

      val assetPath: String =
        flags.getOrElse("baseUrl", "")

      BootResult(config, flicFlacBootData)
        .withAssets(assets)
        .withSubSystems(tyrianSubSystem, SSGame("SubSystemPeerJs"))
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
        GameSceneViewModel.initial,
        flicFlacStartupData.flicFlacBootData.gameViewPort
      )
    )
  end initialViewModel

  def updateViewModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): GlobalEvent => Outcome[FlicFlacViewModel] =
    case FrameTick =>
      Outcome(flicFlacViewModel)

    case ViewportResize(gameViewPort) =>
      val w = gameViewPort.width
      val h = gameViewPort.height
      scribe.debug("@@@ FlicFlacMain-updateViewModel ViewportResize w:h " + w + ":" + h)
      // flicFlacGameModel.hexBoard3.calculateXpYp(1.0) // FIXME this needs to be immutable!!!
      Outcome(flicFlacViewModel.copy(theGameViewPort = gameViewPort))

    case ButtonPlayEvent =>
      scribe.debug("@@@ Main-ButtonGameEvent")
      Outcome(flicFlacViewModel)
        .addGlobalEvents(SceneEvent.JumpTo(SceneGame.name))
        .addGlobalEvents(ViewportResize(flicFlacViewModel.theGameViewPort))

    case _ =>
      Outcome(flicFlacViewModel)
  end updateViewModel

  def updateModel(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] =

    case _ =>
      Outcome(flicFlacGameModel)
  end updateModel

  def present(
      context: FrameContext[FlicFlacStartupData],
      flicFlacGameModel: FlicFlacGameModel,
      flicFlacViewModel: FlicFlacViewModel
  ): Outcome[SceneUpdateFragment] = Outcome {

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
    gameScene: GameSceneViewModel,
    theGameViewPort: GameViewport
)

case object ButtonSplashEvent extends GlobalEvent
case object ButtonRulesEvent extends GlobalEvent
case object ButtonPlayEvent extends GlobalEvent
case object ButtonNewGameEvent extends GlobalEvent
case object ButtonResultsEvent extends GlobalEvent
case object ButtonParamsEvent extends GlobalEvent
case object ButtonPlusEvent extends GlobalEvent
case object ButtonMinusEvent extends GlobalEvent

//ButtonTurnEvent needs to be an object so that it can be filtered and processed in the subsystem(s)
//case object ButtonTurnEvent extends GlobalEvent
object ButtonTurnEvent :
  case class Occurence() extends GlobalEvent
end ButtonTurnEvent

object Freeze:
  case class PanelContent(panelType: PanelType, msg: String) extends GlobalEvent
end Freeze
