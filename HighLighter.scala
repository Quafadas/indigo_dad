package game

import indigo.*

class HighLighter(boardCfg: BoardConfig, hexBoard: HexBoard, fS : Double) :

  var displayOn = false
  val gHex = boardCfg.getHexGraphic() // The Hex graphic used to paint the grid


  var currentPos: Point = Point(0, 0) // testPoint is a simple coordinate inside hexArray

  /*
  setPos repositions the highlighter hex as appropriate
   */
  def setPos(newPos: Point): Unit =
    currentPos = newPos
  end setPos

  /* 
  show enables or disables the display of the HighLighter hex
   */
  def shine(onOff : Boolean) : Unit =
    displayOn = onOff
  end shine

  /*
  paint generates a "SceneUpdateFragment" containing the new position of the Highligter Hex
   */
  def paint(fS : Double): SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty  // the latest fragment for the test hexagon
    if displayOn then
      val pB = boardCfg.pB                // Base Corner (Top LHS) of Rectangle containing board
      val layer = gHex.modifyMaterial(_.withTint(mix(CM)))
      val pPos = hexBoard.getXpYp(currentPos)
      frag = SceneUpdateFragment(Layer(layer.moveTo(pB.x + pPos.x, pB.y + pPos.y).scaleBy(fS, fS)))
    end if
    frag
  end paint

end HighLighter
