package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
case class BoardConfig(

// format: off
    iHexPixelWidth: Int,        // GWIDTH pixel width of graphic
    iHexPixelHeight: Int,       // GHEIGHT pixel height of graphic
    pBase: Point,               // where the (invisible) top left hand corner of the hex grid board is positioned
    iSize: Int,                 // game size
    iWidth: Int,                // amount to add to a hex centre x coord to reach the vertical line of the next column
    iHeight: Int,               // half the amount to add to a hex centre y coord to reach the next hexagon below
    iHalfway: Int               // xcoord of halfway along the top left diagonal line of first hex
// format: on
) derives Encoder.AsObject,
    Decoder:
  println("@@@ ## Class BoardConfig Start")

  val gWidth = iHexPixelWidth - 1 // ..... The graphic will overlap by one pixel because of -1
  val gHeight = iHexPixelHeight - 1 // ... The graphic will overlap by one pixel because of -1

  val pB = pBase
  val sZ = iSize

  val xWidth = iWidth
  val yHeight = iHeight
  val xHalfway = iHalfway

  def getSideSize(): Int =
// format: off
    val sSz = ((sZ - 2) % 5) + 2  // The number of hexagonal rings (ring of 6 with centre) composing one side of the board
    sSz                           // FIXME this formala currently limits 2<=size<=6 where as this should be determined by canvas
// format: on
  end getSideSize

  // Hex for grid on Layer 2
  def getHexGraphic(): Graphic[Material.ImageEffects] =
    println("@@@ ## BoardConfig-getHexGraphic")
    val gHex: Graphic[Material.ImageEffects] =
      GameAssets.gHex
    gHex
  end getHexGraphic

  println("@@@ ## Class BoardConfig Finish")

end BoardConfig
