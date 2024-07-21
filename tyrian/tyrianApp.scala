package game

import tyrian.Html.*
import tyrian.*
import cats.effect.IO
import org.scalajs.dom

import scala.scalajs.js.annotation.*

enum Msg:
  case StartIndigo extends Msg
  case DoNothing extends Msg
  case NavigateToUrl(url: String) extends Msg
end Msg

final case class TyrianModel(bridge: TyrianIndigoBridge[IO, Int, FlicFlacGameModel])
object TyrianModel:
  val init: TyrianModel = TyrianModel(TyrianIndigoBridge())
end TyrianModel

object TyrianApp extends TyrianIOApp[Msg, TyrianModel]:

  // override def init

  override def router: Location => Msg = {
    case loc: Location.Internal =>
      // loc.search
      Msg.DoNothing
    case ext =>
      Msg.NavigateToUrl(ext.href)
  }

  def init(flags: Map[String, String]): (TyrianModel, Cmd[IO, Msg]) =
    (TyrianModel.init, Cmd.Emit(Msg.StartIndigo))

  def update(model: TyrianModel): Msg => (TyrianModel, Cmd[IO, Msg]) = {
    case Msg.StartIndigo =>
      (
        model,
        Cmd.SideEffect {
          HelloIndigo(model.bridge.subSystem(IndigoGameId("indigo-container"))).launch(
            "indigo-container",
            Map[String, String](
              "width" -> dom.window.innerWidth.toString,
              "height" -> dom.window.innerHeight.toString
            )
          )
        }
      )
    case Msg.DoNothing =>
      (model, Cmd.None)
    // format: off
    case Msg.NavigateToUrl(url) => {
        dom.console.error("external navigation not supported")
        (model, Cmd.Emit(Msg.DoNothing))
    }
    // format: on
  }

  end update

  def view(model: TyrianModel): Html[Msg] =
    div(
      h1("Flic Flac!"),
      p("Start the game - beware all ye who enter here!"),
      p("We coudl write out the rules here"),
      p(
        "But I think the best thing, would be to get client side routing working ... so that we can also launch directly to the game, this might be a propblem in deployment though"
      ),
      button(onClick(Msg.StartIndigo))("Start Indigo"),
      div(id := "indigo-container")()
    )

  def subscriptions(model: TyrianModel): Sub[IO, Msg] =
    Sub.None
end TyrianApp
