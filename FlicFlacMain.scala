package game

import indigo.*
import indigo.scenes._
import indigoextras.ui._
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.math._

import org.scalajs.dom

// *******************************************
// Outstanding issues ...
// Browser refresh resets game
// Remove scale factor from Right Mouse Button
// Support Scenes
// Brown Area for Home & perhaps boarder ?
// Rework to ViewModelControl
// *******************************************

@JSExportTopLevel("IndigoGame")
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

object HelloIndigo extends IndigoGame[Unit, Unit, Model, MyViewModel]:
// format: off
  val boardCfg = BoardConfig(
    "hex2",                         // hex asset name
    "assets/Hex2.png",              // path of hex asset
    "bg",                           // background asset name
    "assets/BackGroundWhite.png",   // path of background asset
    91,                             // GWIDTH pixel width of graphic
    81,                             // GHEIGHT pixel height of graphic
    Point(100,50),                  // where the (inisible) top left hand corner of the hex grid board is positioned
    2,                              // game size
    70,                             // amount to add to a hex centre x coord to reach the vertical line of the next column
    40,                             // half the amount to add to a hex centre y coord to reach the next hexagon below
    10                              // xcoord of halfway along the top left diagonal line of first hex
  )

  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  val hexBoard = HexBoard(boardCfg, scaleFactor)
  
  val pieces = Pieces(
    "cylinders",                    // cylinders asset name
    "assets/Cylinders.png",         // path of cylinders asset
    "blocks",                       // blocks asset name
    "assets/Blocks.png",            // path of blocks asset
    boardCfg,
    hexBoard
  )
// format: on

  val highLighter = HighLighter(boardCfg, hexBoard, scaleFactor)

  val magnification = 1

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val assets: Set[AssetType] =
    Set(AssetType.Image(AssetName("TestButtons"), AssetPath("assets/TestButtons.png")))
      ++ boardCfg.getAssets() 
      ++ pieces.getAssets()

  val button1Assets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(0, 0, 80, 40),
      over = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(0, 40, 80, 40),
      down = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(0, 80, 80, 40)
    )

  val button2Assets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(80, 0, 80, 40),
      over = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(80, 40, 80, 40),
      down = Graphic(0, 0, 80, 40, 6, Material.Bitmap(AssetName("TestButtons"))).withCrop(80, 80, 80, 40)
    )

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def setup(
      bootData: Unit,
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
  def initialScene(bootData: Unit): Option[SceneName] = 
    None
  
  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, MyViewModel]] =
    NonEmptyList(Scene.empty)

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit, Model]] =
    Outcome(
      BootResult
        .noData(config)
        .withAssets(assets)
    )

  def initialViewModel(startupData: Unit, model: Model): Outcome[MyViewModel] =
    Outcome(
      MyViewModel(
        button1 = Button( button1Assets, bounds = Rectangle(10,50,80,40), depth = Depth(6) ).withUpActions(AdjustEvent(+0.2)),
        button2 = Button( button2Assets, bounds = Rectangle(10,100,80,40), depth = Depth(6) ).withUpActions(AdjustEvent(-0.2))
      )
    )

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: MyViewModel
  ): GlobalEvent => Outcome[MyViewModel] =
    case FrameTick => 
      for {
            b1 <- viewModel.button1.update(context.inputState.mouse)
            b2 <- viewModel.button2.update(context.inputState.mouse)
      } yield viewModel.copy(button1 = b1, button2 = b2)

    case _ => 
      Outcome(viewModel)
    

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
          val adjustedPosition = clickPoint - model.center
          Outcome(model)
        case _ =>
          println("other detected")
          Outcome(model)
      end match
    //  case MouseEvent.Click(position, buttons) =>
    //    println("This could fire, if right click skipped the first match, but IDE tells me unreachable.")
    //    Outcome(model.copy(center = context.mouse.position))

    case FrameTick =>
      Outcome(model.update(context.delta))

    case e: MouseEvent.MouseUp =>
      e._8 match
        case MouseButton.RightMouseButton =>
          println("MouseEventRightButtonUp @ " + e.position)
          scaleFactor = scaleFactor + 0.2
          if scaleFactor >= 2.1 then scaleFactor = 0.2
          end if
          hexBoard.changeScale(scaleFactor)
          Outcome(model)

        case MouseButton.LeftMouseButton =>
          println("MouseEventLeftButtonUp @ " + e.position)
          val clickPoint = e.position
          val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
          hexPosn match
            // Mouse Up ... The position is on the hex grid
            case Some(pos) =>
              pieces.findPieceSelected() match
                // Mouse Up ... piece selected and valid position
                case Some(piece) =>
                  piece.setPosition(pos)
                  if hexBoard.isThisHexBlack(pos) == true then piece.toggleFlip()
                  end if
                // Mouse Up ... no piece selected but on the grid
                case None => ;

            // Mouse Up ... the position is off the hex grid
            case None =>
              pieces.findPieceSelected() match
                // Mouse Up ... we have selected a piece but moved it off the grid
                case Some(piece) =>
                  piece.moveToHome()

                // Mouse Up ... no piece selected and also off the grid
                case None => ;
          end match
          // Mouse Up so turn highlighter off
          highLighter.shine(false)
          Outcome(model)

        case _ =>
          Outcome(model.update(context.delta))
      end match
    // Outcome(model.update(context.delta))

    case e: MouseEvent.MouseDown =>
      e._8 match
        case MouseButton.LeftMouseButton =>
          println("MouseEventLeftButtonDown @ " + e.position)
          val clickPoint = e.position
          val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
          hexPosn match
            // Mouse Down ... position on the grid
            case Some(pos) =>
              highLighter.setPos(pos)
              highLighter.shine(true)
              pieces.deselectAllPieces()
              pieces.findPieceByPos(pos) match
                // Mouse Down ... piece found and on the grid
                case Some(piece) => piece.setSelected(true)
                case None        => ;
              end match
            // Mouse Down ... position off the grid
            case None => ;
          end match

          Outcome(model)

        case _ =>
          Outcome(model.update(context.delta))

      end match
    // Outcome(model.update(context.delta))

    case AdjustEvent(sFactor) =>
      scaleFactor = scaleFactor + sFactor
      scaleFactor = math.min(scaleFactor,2.0)
      scaleFactor = math.max(scaleFactor,0.2)
      hexBoard.changeScale(scaleFactor)
      Outcome(model)

    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[Unit],
      model: Model,
      viewModel: MyViewModel
  ): Outcome[SceneUpdateFragment] = Outcome {

    val fragsCombined = SceneUpdateFragment.empty |+|
      boardCfg.getBackgroundFrag() |+|
      hexBoard.paint(scaleFactor) |+|
      highLighter.paint(scaleFactor) |+|
      pieces.paint(scaleFactor) |+|
      SceneUpdateFragment(viewModel.button1.draw) |+|
      SceneUpdateFragment(viewModel.button2.draw)

    fragsCombined
  }
end HelloIndigo

// defining a view model that contains UI features for this scene
final case class MyViewModel(button1: Button, button2: Button)

// defining the global event to adjust scale factor. The two test buttons broadcast this
final case class AdjustEvent(scaleF: Double) extends GlobalEvent


final case class Model(center: Point):
  def update(timeDelta: Seconds): Model =
    this.copy()
end Model
object Model:
  def initial(center: Point): Model = Model(center)
end Model
