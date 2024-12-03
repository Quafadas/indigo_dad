package game

import scala.scalajs.js
import indigo.*
import io.github.quafadas.peerscalajs.Peer
import io.github.quafadas.peerscalajs.DataConnection
import scala.collection.mutable.Queue
import io.circe.syntax.*
import io.circe.parser.decode
import cats.effect.kernel.Ref
import cats.effect.unsafe.ref.Reference

final case class SSParams(initialMessage: String) extends SubSystem[FlicFlacGameModel]:

  type EventType = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData = FlicFlacGameModel
  val id: SubSystemId = SubSystemId("SubSystemParams")

  val initMsg = initialMessage

  val eventFilter: GlobalEvent => Option[GlobalEvent] = {
    case e: GlobalEvent => Some(e)
    case null           => None
    }
  def reference(flicFlacGameModel: FlicFlacGameModel): FlicFlacGameModel = flicFlacGameModel

  var subsysPanelMsg: (PanelType, String) = (PanelType.P_INVISIBLE, "")


  def initialModel: Outcome[Unit] =
    scribe.debug("@@@ SubSystemGame initialModel() with " + initMsg)
    Outcome(())
  end initialModel
  
  def update(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): EventType => Outcome[Unit] =
    e =>
      e match
        case Freeze.PanelContent(typeOfPanel,messageToDisplay) =>
          subsysPanelMsg = (typeOfPanel, messageToDisplay)
          scribe.debug("@@@ SubSystemParams FreezePanelContent")
          Outcome(())

        case _ =>
          Outcome(())
  end update

  def present(
      context: SubSystemFrameContext[ReferenceData],
      message: Unit
  ): Outcome[SceneUpdateFragment] =

/*---
    val topXandY = 300
    val boxRed = Shape.Box(Rectangle(topXandY, topXandY, 200, 200), Fill.Color(RGBA.Red)).withDepth(Depth(6)) // ............. (E)
    val boxOrange = Shape.Box(Rectangle(topXandY+4, topXandY+4, 192, 192), Fill.Color(RGBA.Orange)).withDepth(Depth(5)) // ... (F)
    val frag1 = SceneUpdateFragment(BindingKey("Foreground") -> Layer.Content(Batch(boxRed, boxOrange)))

    Outcome (
      SceneUpdateFragment.empty
      |+| frag1
    )
---*/

    subsysPanelMsg  match {
      case (PanelType.P_ERROR, msg) =>
        displayErrorPanel(msg)
      case (PanelType.P_RESULTS, msg) =>
        displayResultsPanel(msg)
      case _ => // including P_INVISIBLE
        Outcome(SceneUpdateFragment.empty)        
    }
  end present

  def displayErrorPanel(msg:String) : Outcome[SceneUpdateFragment] =
    val boxX = 260
    val boxY = 136
    val boxW = (16 + (12 * (msg.length()))).max(1000)
    val boxH = 180

    val borderBox = Shape.Box(Rectangle(boxX, boxY, boxW, boxH), Fill.Color(RGBA.Red))
    val interiorBox = Shape.Box(Rectangle(boxX+4, boxY+4, boxW-8, boxH-8), Fill.Color(RGBA.Cyan))

    val frag1 = SceneUpdateFragment(BindingKey("Foreground") -> Layer.Content(Batch(borderBox, interiorBox)))

    val textError1 = TextBox("*** FlicFlac ERROR ***" , boxW-16, boxH-16)
      .alignCenter.bold.withColor(RGBA.Red).withFontSize(Pixels(60)).moveTo(boxX+8, boxY+8)

    val textError2 = TextBox(msg , boxW-16, boxH-16)
      .withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(boxX+8, boxY+100)

    val textError3 = TextBox("(Click on any part of the white border to dismiss this notification)", boxW-16, boxH-16)
      .withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(boxX+8, boxY+140)

    val frag2 = SceneUpdateFragment(BindingKey("Foreground") -> Layer.Content(Batch(textError1,textError2,textError3)))

   
/*---      
    val topXandY = 250
    val boxOlive = Shape.Box(Rectangle(topXandY, topXandY, 200, 200), Fill.Color(RGBA.Olive)).withDepth(Depth(5))
    val boxCyan = Shape.Box(Rectangle(topXandY+4, topXandY+4, 192, 192), Fill.Color(RGBA.Cyan)).withDepth(Depth(6))
    val frag1 = SceneUpdateFragment(BindingKey("Foreground") -> Layer.Content(Batch(boxOlive, boxCyan)))
---*/

    Outcome (
      SceneUpdateFragment.empty
      |+| frag1
      |+| frag2
    )    



  end displayErrorPanel

  def displayResultsPanel(msg:String) : Outcome[SceneUpdateFragment] = 
    Outcome(SceneUpdateFragment.empty) // FIXME not implemented yet    
  end displayResultsPanel

end SSParams




