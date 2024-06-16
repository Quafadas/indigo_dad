package game

import indigo.*
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.dom
//import scala.collection.mutable.ListBuffer

//------

@JSExportTopLevel("main")
object Game:
  def main(args: Array[String]): Unit =
    HelloIndigo.launch(
      "indigo-container",
      Map[String, String](
        "width" -> dom.window.innerWidth.toString,
        "height" -> dom.window.innerHeight.toString
      )
    )
end Game

// gameSize can be one of 2,3,4,5,6 and is the number of major hexagons across one side where ...
// ... a major hexagon is ring of 6 hexagons, with a central 7th black hexagon
//val gameSize = 2 // <<<<<<<<<<<<<<<<<<<<<<<
//val boardBasePoint : Point = Point(400,50)  // where the (inisible) top left hand corner of the hex grid board is positioned

object HelloIndigo extends IndigoSandbox[Unit, Model]:
  val boardCfg = BoardConfig(
    "hex2", // hex asset name
    "assets/Hex2.png", // path of hex asset
    "bg", // background asset name
    "assets/BackGroundWhite.png", // path of background asset
    91, // GWIDTH pixel width of graphic
    81, // GHEIGHT pixel height of graphic
    Point(
      40,
      50
    ), // where the (inisible) top left hand corner of the hex grid board is positioned
    2, // game size
    70, // amount to add to a hex centre x coord to reach the vertical line of the next column
    40, // half the amount to add to a hex centre y coord to reach the next hexagon below
    10 // xcoord of halfway along the top left diagonal line of first hex
  )

  val hexBoard = HexBoard(boardCfg)

  val pieces = Pieces(
    "cylinders",                // cylinders asset name
    "assets/Cylinders.png",     // path of cylinders asset
    "blocks",                   // blocks asset name
    "assets/Blocks.png",        // path of blocks asset
    boardCfg,
    hexBoard)
    //91, // GWIDTH pixel width of graphic
    //81, // GHEIGHT pixel height of graphic
    //Point(400,50) // where the (invisible) top left hand corner of the hex grid board is positioned
//
//  )

  val magnification = 1

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assets: Set[AssetType] =
    boardCfg.getAssets() ++ pieces.getAssets()

  val fonts: Set[FontInfo] =
    Set()

  val shaders: Set[Shader] =
    Set()

  def setup(
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(
      Model.initial(
        config.viewport.giveDimensions(magnification).center
      )
    )

  def updateModel(
      context: FrameContext[Unit],
      model: Model
  ): GlobalEvent => Outcome[Model] = {
    case e: MouseEvent.Click =>
      println("Mouse click detected")
      println(e._8)
      e.button match
        case MouseButton.RightMouseButton =>
          println("right click - dont' fire")
          Outcome(model.copy(center = e.position))

        case MouseButton.LeftMouseButton =>
          val clickPoint = e.position
          println("MouseClick @ " + e.position)
          val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint)
          hexBoard.testPointReposition(hexPosn)
          val adjustedPosition = clickPoint - model.center
          Outcome(model)
        case _ =>
          println("other detected")
          Outcome(model)
      end match
//      case MouseEvent.Click(position, buttons) =>
//      println("This could fire, if right click skipped the first match, but IDE tells me unreachable.")
//      Outcome(model.copy(center = context.mouse.position))

    case FrameTick =>
      Outcome(model.update(context.delta))

    case e: MouseEvent.MouseUp =>
      e._8 match
        case MouseButton.RightMouseButton =>
          hexBoard.changeScale(0.2)
          pieces.changeScale(0.2)
        case _ =>
          ;
      end match
      Outcome(model.update(context.delta))

    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] = Outcome {

    val fragsCombined = SceneUpdateFragment.empty |+|
      boardCfg.getBackgroundFrag() |+|
      hexBoard.paint() |+|
      hexBoard.paintTestHex() |+|
      pieces.paint()

    fragsCombined
  }

end HelloIndigo
final case class Model(center: Point):
  def update(timeDelta: Seconds): Model =
    this.copy()
end Model
object Model:
  def initial(center: Point): Model = Model(center)
end Model
