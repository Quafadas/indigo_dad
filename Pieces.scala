package game

import indigo.*

val CYLINDER = 0
val BLOCK = 1

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

final case class Pieces(
    modelPieces: Vector[Piece],
):
  println("@@@ Pieces Start")

  /* paint draws the 12 pieces
   */
  def paint(model: FlicFlacGameModel, fS: Double, optDragPos: Option[Point]): SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    
    val pB = model.hexBoard3.pBase // extract GridBasePoint for later

    // first draw all the unselected pieces ...

    for p <- model.pieces.modelPieces do
      val layer = PieceAssets.getGraphic(p.pieceShape, p.pieceIdentity, p.bFlipped)
      val pSrc = p.pCurPos

      if Piece.selected(p) == false then
        val pPos = model.hexBoard3.getXpYp(pSrc)
        val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB + pPos).scaleBy(fS, fS)))
        frag = frag |+| newFrag
      end if
    end for
    // second draw the selected piece if it exists
    // ... (only expecting one for now, but perhaps game might allow more in future)

    for p <- model.pieces.modelPieces do
      if Piece.selected(p) == true then
        val layer = PieceAssets.getGraphic(p.pieceShape, p.pieceIdentity, p.bFlipped)
        val pSrc = p.pCurPos
        optDragPos match
          case Some(pos) =>
            val pC = Point(PieceAssets.gWidth / 2, PieceAssets.gHeight / 2)
            val pPos = pos - pC
            val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pPos).scaleBy(fS, fS)))
            frag = frag |+| newFrag
          case None =>
            val pPos = model.hexBoard3.getXpYp(pSrc)
            val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB + pPos).scaleBy(fS, fS)))
            frag = frag |+| newFrag
        end match
    end for

    frag
  end paint

  println("@@@ Pieces Finish")

end Pieces
