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
val gameSize = 3 // <<<<<<<<<<<<<<<<<<<<<<<

// 
val boardBasePoint : Point = Point(400,50)  // where the (inisible) top left hand corner of the hex grid board is positioned

object HelloIndigo extends IndigoSandbox[Unit, Model]:

  val hexBoard = HexBoard(boardBasePoint, gameSize)
  val hex2Asset = hexBoard.assetName
  val bgAsset   = AssetName("bg")
  val magnification = 1

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(hex2Asset, hexBoard.assetPath),
      AssetType.Image(bgAsset,   AssetPath("assets/BackGroundWhite.png"))
    )

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
    
//    case MouseEvent.Click(position, buttons) =>
//      println("This could fire, if right click skipped the first match, but IDE tells me unreachable.")
//      Outcome(model.copy(center = context.mouse.position))

    case FrameTick =>
      Outcome(model.update(context.delta))

    case e: MouseEvent.MouseUp => 
      e._8 match
        case MouseButton.RightMouseButton =>
          hexBoard.changeScale(0.2)
        case _ =>
          ;
        Outcome(model.update(context.delta))
      
    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[Unit],
      model: Model
      ): Outcome[SceneUpdateFragment] = Outcome {
      
    var fragsCombined = SceneUpdateFragment.empty

    // White background fragment
    val bgGraphic : Graphic[Material.ImageEffects] = Graphic(0,0,256,256,1, Material.ImageEffects(AssetName("bg")))
    val bgLayer = bgGraphic.modifyMaterial(_.withTint(RGBA.White))
    val bgFrag = SceneUpdateFragment(Layer(bgLayer.scaleBy(12,12))) // 12 chosen but needs optimising to scale factor
    fragsCombined = fragsCombined |+| bgFrag

    // Hex board fragments
    fragsCombined = fragsCombined |+| hexBoard.paint()
    fragsCombined = fragsCombined |+| hexBoard.paintTestHex()

    /*
    // Magenta Test Box fragments used to pinpoint positions on the grid and debug
    // the various routines contained in HexBoard.scala

    val bgGraphic2 : Graphic[Material.ImageEffects] = Graphic(0,0,256,256,1, Material.ImageEffects(AssetName("bg")))
    val bgLayer2 = bgGraphic2.modifyMaterial(_.withTint(RGBA.Magenta))

    val zz = hexBoard.fS

    val xLeftUp = 400 + math.round(45*zz).toInt
    val yLeftUp =  50 + math.round(40*zz).toInt
    
    var xRightDown = 500    // dummy values of 500
    var yRightDown = 500    // dummy values of 500

    if (gameSize == 2) then           // size 2 verified
      xRightDown = 400+ math.round((45+(10*70))*zz).toInt
      yRightDown = 50 + math.round((0 + (11*80))*zz).toInt
    else if (gameSize == 3 )          // size 3 verified
      xRightDown = 400+ math.round((45+(16*70))*zz).toInt
      yRightDown = 50 + math.round((0 + (17*80))*zz).toInt
    else if (gameSize == 4 )          // size 4 verified
      xRightDown = 400+ math.round((45+(22*70))*zz).toInt
      yRightDown = 50 + math.round((0 + (23*80))*zz).toInt
    else if (gameSize == 5 )          // size 5 verified
      xRightDown = 400+ math.round((45+(28*70))*zz).toInt
      yRightDown = 50 + math.round((0 + (29*80))*zz).toInt
    else if (gameSize == 6 )          // size 6 verified
      xRightDown = 400+ math.round((45+(34*70))*zz).toInt
      yRightDown = 50 + math.round((0 + (35*80))*zz).toInt
    else
      xRightDown = 500
      yRightDown = 500
    

    val bgFrag1 = SceneUpdateFragment(Layer(bgLayer2.scaleBy(0.1,0.1).moveTo(xLeftUp,yLeftUp)))
    fragsCombined = fragsCombined |+| bgFrag1
    val bgFrag2 = SceneUpdateFragment(Layer(bgLayer2.scaleBy(0.1,0.1).moveTo(xRightDown,yRightDown)))
    fragsCombined = fragsCombined |+| bgFrag2
    val bgFrag3 = SceneUpdateFragment(Layer(bgLayer2.scaleBy(0.1,0.1).moveTo(400+math.round((45+(5*70))*zz).toInt,50+math.round((0+(6*80))*zz).toInt)))
    fragsCombined = fragsCombined |+| bgFrag3

*/
    fragsCombined
  }

end HelloIndigo
final case class Model(center: Point) :
  def update(timeDelta: Seconds) : Model =
    this.copy()
end Model
object Model:
  def initial(center: Point): Model = Model(center)
end Model
