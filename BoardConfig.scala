package game

import indigo.*

class BoardConfig(
    sHexAssetName: String, // hex asset name
    sHexAssetPath: String, // path of hex asset
    sBgrndAssetName: String, // background asset name
    sBgrndAssetPath: String, // path of background asset
    iHexPixelWidth: Int, // GWIDTH pixel width of graphic
    iHexPixelHeight: Int, // GHEIGHT pixel height of graphic
    pBase: Point, // where the (invisible) top left hand corner of the hex grid board is positioned
    iSize: Int, // game size
    iWidth: Int, // amount to add to a hex centre x coord to reach the vertical line of the next column
    iHeight: Int, // half the amount to add to a hex centre y coord to reach the next hexagon below
    iHalfway: Int // xcoord of halfway along the top left diagonal line of first hex
):

  val hexAssetName = AssetName(sHexAssetName)
  val hexAssetPath = AssetPath(sHexAssetPath)

  val bgrndAssetName = AssetName(sBgrndAssetName)
  val bgrndAssetPath = AssetPath(sBgrndAssetPath)

  val gWidth =
    iHexPixelWidth - 1 // The graphic will overlap by one pixel because of -1
  val gHeight =
    iHexPixelHeight - 1 // The graphic will overlap by one pixel because of -1

  val pB = pBase
  val sZ = iSize

  val xWidth = iWidth
  val yHeight = iHeight
  val xHalfway = iHalfway

  def getAssets(): Set[AssetType] =
    Set(
      AssetType.Image(hexAssetName, hexAssetPath),
      AssetType.Image(bgrndAssetName, bgrndAssetPath)
    )

  def getSideSize(): Int =
    val sSz = ((sZ - 2) % 5) + 2  // The number of hexagonal rings (ring of 6 with centre) composing one side of the board
    sSz                           // FIXME this formala currently limits 2<=size<=6 where as this should be determined by canvas
  end getSideSize

  // White background fragment
  def getBackgroundFrag(): SceneUpdateFragment =
    val bgGraphic: Graphic[Material.ImageEffects] = Graphic(0, 0, 256, 256, 1, Material.ImageEffects(bgrndAssetName))
    val bgFrag = SceneUpdateFragment(Layer(bgGraphic.scaleBy(12, 12))) // 12 chosen but needs optimising to scale factor
    bgFrag
  end getBackgroundFrag

  // Hex for grid on Layer 2
  def getHexGraphic(): Graphic[Material.ImageEffects] =
    val rHex = Rectangle(0, 0, gWidth + 1, gHeight + 1)
    val gHex: Graphic[Material.ImageEffects] =
      Graphic(rHex,2,Material.ImageEffects(hexAssetName)) 
    gHex
  end getHexGraphic

end BoardConfig
