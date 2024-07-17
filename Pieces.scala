package game

import indigo.*

val CYLINDER = false
val BLOCK = true

// First 6 colors are used in modulo 6 fashion for pieces
val CB = 0 // CB for Blue
val CG = 1 // CR for Green
val CY = 2 // CG for Yellow
val CO = 3 // CY for Orange
val CR = 4 // CO for Red
val CP = 5 // CP for Purple
//-----------------------------------------------------
val CK = 6 // CK for Black
val CW = 7 // CW for White
val CC = 8 // CC for Cyan is a test color
val CM = 9 // CM for Magenta is a test color
val CX = 10 // CX indicates hex does not so not visible (indicated by transparency field = 0)

def mix(i: Int): RGBA =
  i match
    case CX => RGBA.fromHexString("#00000000") // Zero
    case CB => RGBA.fromHexString("#80C0FFFF") // Blue
    case CG => RGBA.fromHexString("#C0FFC0FF") // Green
    case CY => RGBA.fromHexString("#FFFFC0FF") // Yellow
    case CO => RGBA.fromHexString("#FFD070FF") // Orange
    case CR => RGBA.fromHexString("#FFC0C0FF") // Red
    case CP => RGBA.fromHexString("#CCCCFFFF") // Purple
    case CK => RGBA.fromHexString("#808080FF") // Black
    case CW => RGBA.fromHexString("#FFFFFFFF") // White
    case CC => RGBA.fromHexString("#00FFFFFF") // Cyan
    case CM => RGBA.fromHexString("#FF00FFFF") // Magenta
    case _  => RGBA.fromHexString("#FF00FFFF") // Magenta

class Pieces(
    //boardCfg: BoardConfig,
    hexBoard: HexBoard
):
  println("@@@ Pieces Start")
/*
  val pB = boardCfg.pB
  val w = boardCfg.gWidth
  val h = boardCfg.gHeight

  val pieces: Vector[Piece] = Vector(
    Piece(CYLINDER, CB, hexBoard.getCylinderHomePos(CB), hexBoard.getCylinderHomePos(CB)),
    Piece(CYLINDER, CG, hexBoard.getCylinderHomePos(CG), hexBoard.getCylinderHomePos(CG)),
    Piece(CYLINDER, CY, hexBoard.getCylinderHomePos(CY), hexBoard.getCylinderHomePos(CY)),
    Piece(CYLINDER, CO, hexBoard.getCylinderHomePos(CO), hexBoard.getCylinderHomePos(CO)),
    Piece(CYLINDER, CR, hexBoard.getCylinderHomePos(CR), hexBoard.getCylinderHomePos(CR)),
    Piece(CYLINDER, CP, hexBoard.getCylinderHomePos(CP), hexBoard.getCylinderHomePos(CP)),
    Piece(BLOCK, CB, hexBoard.getBlockHomePos(CB), hexBoard.getBlockHomePos(CB)),
    Piece(BLOCK, CG, hexBoard.getBlockHomePos(CG), hexBoard.getBlockHomePos(CG)),
    Piece(BLOCK, CY, hexBoard.getBlockHomePos(CY), hexBoard.getBlockHomePos(CY)),
    Piece(BLOCK, CO, hexBoard.getBlockHomePos(CO), hexBoard.getBlockHomePos(CO)),
    Piece(BLOCK, CR, hexBoard.getBlockHomePos(CR), hexBoard.getBlockHomePos(CR)),
    Piece(BLOCK, CP, hexBoard.getBlockHomePos(CP), hexBoard.getBlockHomePos(CP))
  )
*/
  /* paint draws the 12 pieces
   */
  def paint(model: FlicFlacGameModel, fS: Double, optDragPos: Option[Point]): SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    val pB = model.boardConfig.pB // extract GridBasePoint for later 

    // first draw all the unselected pieces ...

    for ( p <- model.modelPieces )
      val layer = PieceAssets.getGraphic(p.pieceShape, p.pieceIdentity, p.bFlipped)
      val pSrc = p.pCurPos

      if (Piece.selected(p) == false) then
        val pPos = hexBoard.getXpYp(pSrc)
        val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB + pPos).scaleBy(fS, fS)))
        frag = frag |+| newFrag
    end for

    // second draw the selected piece if it exists
    // ... (only expecting one for now, but perhaps game might allow more in future)   

    for ( p <- model.modelPieces )
      if (Piece.selected(p) == true) then 
        val layer = PieceAssets.getGraphic(p.pieceShape, p.pieceIdentity, p.bFlipped)
        val pSrc = p.pCurPos
        optDragPos match 
          case Some(pos) => 
            val pC = Point(PieceAssets.gWidth/2, PieceAssets.gHeight/2 )
            val pPos = pos - pC
            val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pPos).scaleBy(fS, fS)))
            frag = frag |+| newFrag
          case None => 
            val pPos = hexBoard.getXpYp(pSrc)
            val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB + pPos).scaleBy(fS, fS)))
            frag = frag |+| newFrag
        end match
    end for

    frag
  end paint
/*
  def findPieceByPos(pos: Point): Option[Piece] =
    pieces.find(Piece.position(_) == pos)
  end findPieceByPos

  def findPieceSelected(): Option[Piece] =
    pieces.find(Piece.selected(_) == true)
  end findPieceSelected

  def deselectAllPieces(): Unit =
  for (p <- pieces)
    if Piece.selected(p) then
      Piece.setSelected(p,false)    // FIXME this now needs to record the new piece
  end deselectAllPieces
*/
  println("@@@ Pieces Finish")

end Pieces
