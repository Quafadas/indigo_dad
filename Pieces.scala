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

val pieceNames: Array[String] = Array(
  "Blue",
  "Green",
  "Yellow",
  "Orange",
  "Red",
  "Purple"
)

class Pieces(
    sCylindersAssetName: String,
    sCylindersAssetPath: String,
    sBlocksAssetName: String,
    sBlocksAssetPath: String,
    boardCfg: BoardConfig,
    hexBoard: HexBoard
):

  val cylindersAssetName = AssetName(sCylindersAssetName)
  val cylindersAssetPath = AssetPath(sCylindersAssetPath)
  val blocksAssetName = AssetName(sBlocksAssetName)
  val blocksAssetPath = AssetPath(sBlocksAssetPath)
  val pB = boardCfg.pB
  val w = boardCfg.gWidth
  val h = boardCfg.gHeight

  val pieces: Vector[Piece] = Vector(
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CB), CB, w, h, cylindersAssetName),
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CG), CG, w, h, cylindersAssetName),
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CY), CY, w, h, cylindersAssetName),
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CO), CO, w, h, cylindersAssetName),
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CR), CR, w, h, cylindersAssetName),
    Piece(CYLINDER, hexBoard.getCylinderHomePos(CP), CP, w, h, cylindersAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CB), CB, w, h, blocksAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CG), CG, w, h, blocksAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CY), CY, w, h, blocksAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CO), CO, w, h, blocksAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CR), CR, w, h, blocksAssetName),
    Piece(BLOCK, hexBoard.getBlockHomePos(CP), CP, w, h, blocksAssetName)
  )

  def getAssets(): Set[AssetType] =
    Set(
      AssetType.Image(cylindersAssetName, cylindersAssetPath),
      AssetType.Image(blocksAssetName, blocksAssetPath)
    )
  end getAssets

  /* paint draws the 12 pieces and the magenta highligter if one piece is selected
   */
  def paint(fS: Double): SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    // traverse the pieces in reverse order, so that last displayed matches first found
    var p = pieces.length - 1
    while p >= 0 do
      val pSrc = pieces(p).pCurPos
      val pPos = hexBoard.getXpYp(pSrc)
      val layer = pieces(p).getGraphic()
      val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB.x + pPos.x, pB.y + pPos.y).scaleBy(fS, fS)))
      frag = frag |+| newFrag
      p -= 1
    end while
    frag
  end paint

  def findPieceByPos(pos: Point): Option[Piece] =
    pieces.find(_.position() == pos)
  end findPieceByPos

  def findPieceSelected(): Option[Piece] =
    pieces.find(_.selected() == true)
  end findPieceSelected

  def deselectAllPieces(): Unit =
    var p = 0
    while p < pieces.length do
      pieces(p).setSelected(false)
      p += 1
    end while
  end deselectAllPieces

end Pieces
