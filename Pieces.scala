package game

import indigo.*

val CYLINDER = false
val BLOCK    = true

// First 6 colors are used in modulo 6 fashion for pieces
val CB = 0 // CB for Blue
val CG = 1 // CR for Red
val CY = 2 // CG for Green
val CO = 3 // CY for Yellow
val CR = 4 // CO for Orange
val CP = 5 // CP for Purple
//-----------------------------------------------------
val CK = 6 // CK for Black
val CW = 7 // CW for White
val CC = 8 // CC for Cyan is a test color
val CM = 9 // CM for Magenta is a test color
val CX = 10 // CX indicates hex does not so not visible (indicated by transparency field = 0)

def mix(i: Int): RGBA =
  i match
    case CX => RGBA.fromHexString("#00000000")  // Zero
    case CB => RGBA.fromHexString("#80C0FFFF")  // Blue
    case CR => RGBA.fromHexString("#FFC0C0FF")  // Red
    case CG => RGBA.fromHexString("#C0FFC0FF")  // Green
    case CY => RGBA.fromHexString("#FFFFC0FF")  // Yellow
    case CO => RGBA.fromHexString("#FFD070FF")  // Orange
    case CP => RGBA.fromHexString("#CCCCFFFF")  // Purple
    case CK => RGBA.fromHexString("#808080FF")  // Black
    case CW => RGBA.fromHexString("#FFFFFFFF")  // White
    case CC => RGBA.fromHexString("#00FFFFFF")  // Cyan
    case CM => RGBA.fromHexString("#FF00FFFF")  // Magenta
    case _  => RGBA.fromHexString("#FF00FFFF")  // Magenta

val pieceNames: Array[String] = Array(
  "Blue",
  "Red",
  "Green",
  "Yellow",
  "Orange",
  "Purple"
)

class Piece(  pieceShape: Boolean, 
              pieceHome: Point, 
              pieceIdentity: Int,
              iHexPixelWidth: Int,  // GWIDTH pixel height of graphic
              iHexPixelHeight: Int, // GHEIGHT pixel height of graphic
              assetName: AssetName  // The asset name of the graphic containing the images for the pieces 
):
  val pieceType = pieceShape          // true=block, false=cylinder
  val homePos: Point = pieceHome      // home position (in hexArray coords)
  val id: Int = pieceIdentity % 6     // piece Id (modulo 6 ... 0,1,2,3,4,5)
  val gWidth = iHexPixelWidth
  val gHeight = iHexPixelHeight
  
  var flipped = 0                     // piece normal is 0, piece flipped is 1
  var captured = false                // piece is captured (or not)
  var curPos: Point = pieceHome       // current position (in hexArrayCoords)
  val name = pieceNames(pieceIdentity % 6)

  val rNormal = Rectangle(gWidth * id, 0, gWidth + 1, gHeight + 1)
  val gNormal = Graphic( rNormal, 4, Material.ImageEffects(assetName) ) // Pieces on Layer 4

  val rFlipped = Rectangle(gWidth * id, gHeight, gWidth + 1, gHeight + 1)
  val gFlipped = Graphic(rFlipped, 4, Material.ImageEffects(assetName) ) // Pieces on Layer 4

  
  def getGraphic () : Graphic[Material.ImageEffects] =
    var graphic = gNormal
    if (flipped > 0)
      graphic = gFlipped
    graphic

end Piece

class Pieces (  sCylindersAssetName: String,
                sCylindersAssetPath: String,
                sBlocksAssetName: String,
                sBlocksAssetPath: String,
                boardCfg : BoardConfig,
                hexBoard : HexBoard
              ) :

  val cylindersAssetName = AssetName(sCylindersAssetName)
  val cylindersAssetPath = AssetPath(sCylindersAssetPath)
  val blocksAssetName = AssetName(sBlocksAssetName)
  val blocksAssetPath = AssetPath(sBlocksAssetPath)
  val pB = boardCfg.pB
  val w = boardCfg.gWidth
  val h = boardCfg.gHeight
  var fS = 1.0 // scale factor

  val pieces : Vector[Piece] = Vector(
    Piece(CYLINDER, Point(2,9), CB, w,h, cylindersAssetName),
    Piece(CYLINDER, Point(2,11), CG, w,h, cylindersAssetName),
    Piece(CYLINDER, Point(2,13), CY, w,h, cylindersAssetName),
    Piece(CYLINDER, Point(1,10), CO, w,h, cylindersAssetName),
    Piece(CYLINDER, Point(1,7), CR, w,h, cylindersAssetName),
    Piece(CYLINDER, Point(1,14), CP, w,h, cylindersAssetName),

    Piece(BLOCK,    Point(3,5), CB, w,h, blocksAssetName),
    Piece(BLOCK,    Point(3,7), CG, w,h, blocksAssetName),
    Piece(BLOCK,    Point(3,9), CY, w,h, blocksAssetName),
    Piece(BLOCK,    Point(4,8), CO, w,h, blocksAssetName),
    Piece(BLOCK,    Point(4,11), CR, w,h, blocksAssetName),
    Piece(BLOCK,    Point(4,12), CP, w,h, blocksAssetName)
    )

  def getAssets(): Set[AssetType] =
    Set(
      AssetType.Image(cylindersAssetName, cylindersAssetPath),
      AssetType.Image(blocksAssetName, blocksAssetPath)
    )


  /* paint draws the 12 pieces and the magenta highligter if one piece is selected
  */
  
  def paint(): SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    var p = 0
    while p < pieces.length do
      val x = pieces(p).curPos.x
      val y = pieces(p).curPos.y
      val xP = hexBoard.hexArray(x)(y).xP
      val yP = hexBoard.hexArray(x)(y).yP

      val layer = pieces(p).getGraphic()
      //println("piecesPaint xP:yP " + xP )
      // under construction
      /*
      val layer = gHex.modifyMaterial(_.withTint(mix(CM)))
      val xP = hexArray(testPoint.x)(testPoint.y).xP
      val yP = hexArray(testPoint.x)(testPoint.y).yP
      testFrag = SceneUpdateFragment(Layer(layer.moveTo(pB.x + xP, pB.y + yP).scaleBy(fS, fS)))
      */
      
      val newFrag = SceneUpdateFragment(Layer(layer.moveTo(pB.x + xP, pB.y + yP).scaleBy(fS, fS)))
      frag = frag |+| newFrag
      p += 1
    end while
    frag

  def changeScale(change: Double): Unit =
    fS = fS + change
    if fS >= 2 then fS = 0.2
    end if
  end changeScale

end Pieces