package game

import indigo.*

final case class HighLighter(
    val hexBoard3: HexBoard3,
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

  def paint(model: FlicFlacGameModel, fS: Double): Layer =
    var highLighterLayer = Layer.empty
    if model.highLighter.displayOn then
      val pB = model.hexBoard3.pBase // ................. Base Corner (Top LHS) of Rectangle containing board
      val pPos = model.hexBoard3.getXpYp(currentPos)
      val layer = GameAssets.gHex(fS).modifyMaterial(_.withTint(mix(CM)))
      highLighterLayer = Layer(layer.moveTo(pB.x + pPos.x, pB.y + pPos.y))
    end if
    highLighterLayer
  end paint

end HighLighter
