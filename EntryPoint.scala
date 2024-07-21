package game

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object Game:
  def main(args: Array[String]): Unit =
    scribe.info("@@@ TyrianApp launch")
    TyrianApp.launch("myapp")
    scribe.info("Tyrian App Finish")
  end main
end Game
