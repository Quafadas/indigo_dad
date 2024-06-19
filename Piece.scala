package game

import indigo.*

class Piece(  pieceShape: Boolean, 
              pieceHome: Point, 
              pieceIdentity: Int,
              iHexPixelWidth: Int,  // GWIDTH pixel height of graphic
              iHexPixelHeight: Int, // GHEIGHT pixel height of graphic
              assetName: AssetName  // The asset name of the graphic containing the images for the pieces 
) :
  val bType = pieceShape              // true=block, false=cylinder
  val homePos: Point = pieceHome      // home position (in hexArray coords)
  val id: Int = pieceIdentity % 6     // piece Id (modulo 6 ... 0,1,2,3,4,5)
  val gWidth = iHexPixelWidth
  val gHeight = iHexPixelHeight
  
  var bFlipped = false                // piece normal is 0, piece flipped is 1
  var bSelected = false               // piece is selected
  var bCaptured = false               // piece is captured (or not)
  var pCurPos: Point = pieceHome      // current position (in hexArrayCoords)
  val sName = pieceNames(pieceIdentity % 6)

  val rNormal = Rectangle(gWidth * id, 0, gWidth + 1, gHeight + 1)
  val gNormal = Graphic( rNormal, 4, Material.ImageEffects(assetName) ) // Pieces on Layer 4

  val rFlipped = Rectangle(gWidth * id, gHeight, gWidth + 1, gHeight + 1)
  val gFlipped = Graphic(rFlipped, 4, Material.ImageEffects(assetName) ) // Pieces on Layer 4

  
  def getGraphic () : Graphic[Material.ImageEffects] =
    var graphic = gNormal
    if bFlipped then
      graphic = gFlipped
    end if
    graphic

  def setSelected(b: Boolean) : Unit = 
    if (b) then
      println(sName + " selected:" + b)
    bSelected = b

  def selected() : Boolean = 
    bSelected

  def setFlipped(b: Boolean) : Unit = 
    bFlipped = b
  
  def toggleFlip() : Unit = 
    if bFlipped then
      bFlipped = false
    else
      bFlipped = true
    end if

  def flipped() : Boolean = 
    bFlipped

  def setCaptured(b: Boolean) : Unit = 
    bCaptured = b

  def captured() : Boolean = 
    bCaptured

  def setPosition(pPos : Point) : Unit = 
    pCurPos = pPos

  def position() : Point = 
    pCurPos

  def pieceType() : Boolean = 
    bType
  
  def pieceId() : Int = 
    id

  def pieceName() : String = 
    sName

end Piece
