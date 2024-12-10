package game

import indigo.*

final case class HighLighter(
    val displayOn: Boolean,
    val currentPos: Point
):

  /*
  setPos repositions the highlighter hex as appropriate
   */
  def setPosAndShine(highLighter: HighLighter, newPos: Point): HighLighter =
    highLighter.copy(displayOn = true, currentPos = newPos)
  end setPosAndShine

  /*
  show enables or disables the display of the HighLighter hex
   */
  def shine(highLighter: HighLighter, onOff: Boolean): HighLighter =
    highLighter.copy(displayOn = onOff)
  end shine

  /*
  paint generates a "SceneUpdateFragment" containing the new position of the Highligter Hex
   */

  def paint(model: FlicFlacGameModel, fS: Double, pB: Point): Layer =
    if model.highLighter.displayOn then
      val layer = GameAssets.gHex(fS).modifyMaterial(_.withTint(mix(CM)))
      Layer(layer.moveTo(pB.x + currentPos.x, pB.y + currentPos.y))
    else
      Layer.empty
    end if
  end paint

end HighLighter
