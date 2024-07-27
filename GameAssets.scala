package game

import indigo.*
import indigoextras.ui.*
import indigo.shared.materials
import indigo.shared.materials.Material.ImageEffects

object GameAssets:

  val hxAssetName = "hex2"
  val hxAssetPath = "assets/Hex2.png"
  val bnAssetName = "buttons"
  val bnAssetPath = "assets/ButtonGraphics.png"
  val cyAssetName = "cylinders"
  val cyAssetPath = "assets/Cylinders.png"
  val blAssetName = "blocks"
  val blAssetPath = "assets/Blocks.png"
  val spAssetName = "splashBg"
  val spAssetPath = "assets/FlicFlacV4.png"
  val cnAssetName = "corners"
  val cnAssetPath = "assets/Corners.png"
  val rlAssetName = "rules"
  val rlAssetPath = "assets/Rules.png"


  scribe.debug("@@@ Object GameAssets START")

  val SplashSceneDimensions = Rectangle(0, 0, 1920,1080)
  val RulesSceneDimensions = Rectangle(0, 0, 1700, 1250)
  /* Calculating the game rectangle was non-trivial (and not perfect)...
  ... factors taking into consideration were ...
  1) HexBoard3.pBase = Point(260,0)
  2) hh(8)(33) Point(.xR & .yR)=Point(1190,1320) ... watch out 1320 is the x coord
  3) additional margins for half hex in both directions
  Tested on the scale Factors 1.0, 0.9, 0.8, 0.75, 0.5, 0.33, 0.25
  */
  val GameSceneDimensions = Rectangle(0,0,1580,1450)      
  
  def get(): Set[AssetType] =
    Set(
      AssetType.Image(AssetName(hxAssetName), AssetPath(hxAssetPath)),
      AssetType.Image(AssetName(bnAssetName), AssetPath(bnAssetPath)),
      AssetType.Image(AssetName(cyAssetName), AssetPath(cyAssetPath)),
      AssetType.Image(AssetName(blAssetName), AssetPath(blAssetPath)),
      AssetType.Image(AssetName(spAssetName), AssetPath(spAssetPath)),
      AssetType.Image(AssetName(cnAssetName), AssetPath(cnAssetPath)),
      AssetType.Image(AssetName(rlAssetName), AssetPath(rlAssetPath)))

  def gHex(sf: Double): Graphic[Material.ImageEffects] =
    if (sf < 0.25) || (sf > 1.0) then               // FIXME we should not need this trap in the end
      scribe.error ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      scribe.error ("@@@ BAD sf:" + sf)
      scribe.error ("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      Graphic(Rectangle(0, 0, 91, 81), 2, Material.ImageEffects(AssetName(hxAssetName)))//.scaleBy(sf,sf)
    else
      Graphic(Rectangle(0, 0, 91, 81), 2, Material.ImageEffects(AssetName(hxAssetName))).scaleBy(sf,sf)

  // Check FlacFont.txt for details

  def buttonSplashAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 0, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 80, 240, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 160, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up,over,down)

  def buttonParamsAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 0, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 80, 240, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 160, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonPlayAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 0, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 80, 240, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 160, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonResultsAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 0, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 80, 240, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 160, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonRulesAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 240, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 320, 240, 80).scaleBy(sf,sf)
    val down  = Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 400, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonNewGameAssets(sf: Double): ButtonAssets =
    val up =  Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 240, 240, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 320, 240, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 400, 240, 80).scaleBy(sf,sf)
    ButtonAssets(up, over,down)

  def buttonPlusAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 0, 90, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 80, 90, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 160, 90, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonMinusAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 240, 90, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 320, 90, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 400, 90, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)

  def buttonTurnAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 0, 90, 80).scaleBy(sf,sf)
    val over = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 80, 90, 80).scaleBy(sf,sf)
    val down = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 160, 90, 80).scaleBy(sf,sf)
    ButtonAssets(up, over, down)



  def splashBg: Graphic[Material.ImageEffects] =
    Graphic(SplashSceneDimensions, 2, Material.ImageEffects(AssetName(spAssetName)))

  def cornerTopLeft: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerTopRight: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomLeft: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomRight: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerLayers(r: Rectangle, sf: Double, cornerColor: RGBA): Batch[Graphic[ImageEffects]] =
    val newWidth20 = ((r.width-20).toDouble * sf).toInt
    val newHeight20 = ((r.height-20).toDouble *sf).toInt
    val layerC1 = (GameAssets.cornerTopLeft)
      .moveTo(r.left,r.top)
      .modifyMaterial(_.withTint(cornerColor))
    val layerC2 = (GameAssets.cornerTopRight)
      .moveTo(newWidth20, r.top)
      .modifyMaterial(_.withTint(cornerColor))
    val layerC3 = (GameAssets.cornerBottomLeft)
      .moveTo(r.left, newHeight20)
      .modifyMaterial(_.withTint(cornerColor))
    val layerC4 = (GameAssets.cornerBottomRight)
      .moveTo(newWidth20, newHeight20)
      .modifyMaterial(_.withTint(cornerColor))

    Batch(layerC1, layerC2, layerC3, layerC4)
  end cornerLayers

  def rulesBg: Graphic[Material.ImageEffects] =
    Graphic(RulesSceneDimensions, 2, Material.ImageEffects(AssetName(rlAssetName)))

  def scaleButtonBounds(r: Rectangle, sf: Double): Rectangle = 
    val iXpos = (r.position.x * sf).toInt
    val iYpos = (r.position.y * sf).toInt
    val iWidth = (r.width * sf).toInt
    val iHeight = (r.height * sf).toInt
    val rBounds = Rectangle(iXpos, iYpos, iWidth, iHeight)
    rBounds
  end scaleButtonBounds


  scribe.debug("@@@ Object GameAssets FINISH")
end GameAssets
