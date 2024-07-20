package game

import indigo.*
import indigoextras.ui.*

object FlicFlacStartupData:

  println("@@@ Object FlicFlacStartupData START")

  def initialise(
      flicFlacBootData: FlicFlacBootData
  ): Outcome[Startup[FlicFlacStartupData]] =
    Outcome(
      Startup.Success(createStartupData(flicFlacBootData))
    )

  def createStartupData(flicFlacBootData: FlicFlacBootData): FlicFlacStartupData =
    FlicFlacStartupData(
      flicFlacBootData = flicFlacBootData,
      staticAssets = StaticAssets(
        hexGraphic = GameAssets.gHex,
        buttonSplashAsset = GameAssets.buttonSplashAssets(1.0),
        buttonRulesAsset = GameAssets.buttonRulesAssets(1.0),
        buttonPlayAsset = GameAssets.buttonPlayAssets(1.0),
        buttonResultsAsset = GameAssets.buttonResultsAssets(1.0)
      )
    )
  println("@@@ Object FlicFlacStartupData FINISH")
end FlicFlacStartupData

final case class FlicFlacStartupData(flicFlacBootData: FlicFlacBootData, staticAssets: StaticAssets)

final case class StaticAssets(
    hexGraphic: Graphic[Material.ImageEffects],
    buttonSplashAsset: ButtonAssets,
    buttonRulesAsset: ButtonAssets,
    buttonPlayAsset: ButtonAssets,
    buttonResultsAsset: ButtonAssets
)

final case class FlicFlacBootData(pixelWidth: Int, pixelHeight: Int, viewport: GameViewport):
  val width = pixelWidth
  val height = pixelHeight
  val gameViewPort = viewport
end FlicFlacBootData

object FlicFlacBootData:
  println("@@@ Object FlicFlacBootData START")

  def create(w: Int, h: Int): FlicFlacBootData =
    FlicFlacBootData(w, h, GameViewport(w, h))

  println("@@@ Object FlicFlacBootData FINISH")
end FlicFlacBootData

object FlicFlacConfig:
  println("@@@ Object FlicFlacConfig START")
  val config: GameConfig =
    GameConfig(
      viewport = GameViewport(720, 516),
      frameRateLimit = Option(FPS.`60`),
      clearColor = RGBA.fromHexString("#000000"),
      magnification = 1,
      transparentBackground = false,
      resizePolicy = ResizePolicy.ResizePreserveAspect,
      advanced = AdvancedGameConfig(
        renderingTechnology = RenderingTechnology.WebGL2WithFallback,
        antiAliasing = false,
        premultipliedAlpha = true,
        batchSize = 256,
        autoLoadStandardShaders = true,
        disableContextMenu = true
      )
    )
  println("@@@ Object FlicFlacConfig FINISH")
end FlicFlacConfig
