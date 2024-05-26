package game

import indigo.*
import indigo.shared.assets.AssetType
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.events.MouseEvent
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.dom

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

object HelloIndigo extends IndigoSandbox[Unit, Model]:

  val dotsAsset = AssetName("dots")
  val magnification = 1

  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(dotsAsset, AssetPath("assets/dots.png"))
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
      println("but right click doesn't fire")
      println(e)
      e.button match
        case MouseButton.RightMouseButton =>
          println("right click - dont' fire")
          Outcome(model.copy(center = e.position))

        case MouseButton.LeftMouseButton =>
          val clickPoint = e.position
          val adjustedPosition = clickPoint - model.center
          println("add floating ball")
          Outcome(
            model.addDot(
              Dot(
                Point.distanceBetween(model.center, clickPoint).toInt,
                Radians(
                  Math.atan2(
                    adjustedPosition.x.toDouble,
                    adjustedPosition.y.toDouble
                  )
                )
              )
            )
          )
        case _ =>
          println("other detected")
          Outcome(model)
      end match
    case MouseEvent.Click(position, buttons) =>
      println("This could fire, if right click skipped the first match, but IDE tells me unreachable.")
      Outcome(model.copy(center = context.mouse.position))

    case FrameTick =>
      Outcome(model.update(context.delta))

    case _ =>
      Outcome(model)
  }

  def present(
      context: FrameContext[Unit],
      model: Model
  ): Outcome[SceneUpdateFragment] = Outcome {

    SceneUpdateFragment(
      Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(dotsAsset)) ::
        drawDots(model.center, model.dots)
    )
  }

  def drawDots(
      center: Point,
      dots: Batch[Dot]
  ): Batch[Graphic[?]] =
    dots.map { dot =>
      val position = Point(
        (Math.sin(dot.angle.toDouble) * dot.orbitDistance + center.x).toInt,
        (Math.cos(dot.angle.toDouble) * dot.orbitDistance + center.y).toInt
      )

      Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(dotsAsset))
        .withCrop(Rectangle(16, 16, 16, 16))
        .withRef(8, 8)
        .moveTo(position)
    }
end HelloIndigo

final case class Model(center: Point, dots: Batch[Dot]):
  def addDot(dot: Dot): Model =
    this.copy(dots = dot :: dots)

  def update(timeDelta: Seconds): Model =
    this.copy(dots = dots.map(_.update(timeDelta)))
end Model
object Model:
  def initial(center: Point): Model = Model(center, Batch.empty)
end Model

final case class Dot(orbitDistance: Int, angle: Radians):
  def update(timeDelta: Seconds): Dot =
    this.copy(angle = angle + Radians.fromSeconds(timeDelta) / 6)
end Dot
