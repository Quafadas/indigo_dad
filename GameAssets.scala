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
  val cnAssetName = "corners"
  val cnAssetPath = "assets/Corners.png"
  val spAssetName = "scorePanel"
  val spAssetPath = "assets/ScorePanelx50.png"
  val tsAssetName = "timeSlider"
  val tsAssetPath = "assets/TimeSlider.png"
  val pmAssetName = "paramsPanel"
  val pmAssetPath = "assets/ParamsPanelx50.png"

  scribe.debug("@@@ Object GameAssets START")

  /* Game rectangle found by experimentation.
     This rectangle dimensions also laid out in html file
     This is the "master" setting for the game dimensions 
     for all of the scala code
  */

  val GameSceneDimensions = Rectangle(0, 0, 1580, 1300)

  def get(): Set[AssetType] =
    Set(
      AssetType.Image(AssetName(hxAssetName), AssetPath(hxAssetPath)),
      AssetType.Image(AssetName(bnAssetName), AssetPath(bnAssetPath)),
      AssetType.Image(AssetName(cyAssetName), AssetPath(cyAssetPath)),
      AssetType.Image(AssetName(blAssetName), AssetPath(blAssetPath)),
      AssetType.Image(AssetName(cnAssetName), AssetPath(cnAssetPath)),
      AssetType.Image(AssetName(spAssetName), AssetPath(spAssetPath)),
      AssetType.Image(AssetName(tsAssetName), AssetPath(tsAssetPath)),
      AssetType.Image(AssetName(pmAssetName), AssetPath(pmAssetPath))
    )

  def gHex(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 91, 81), 2, Material.ImageEffects(AssetName(hxAssetName))).scaleBy(sf, sf)
  end gHex

  def gSpot(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 91, 81), 3, Material.ImageEffects(AssetName(hxAssetName)))
      .withCrop(100, 0, 91, 81)
      .scaleBy(sf, sf)

  def gScorePanelBlinkOff(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 250, 440), 3, Material.ImageEffects(AssetName(spAssetName)))

  def gScorePanelBlinkCylinder(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(250, 0, 250, 440), 3, Material.ImageEffects(AssetName(spAssetName)))

  def gScorePanelBlinkBlock (sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(500, 0, 250, 440), 3, Material.ImageEffects(AssetName(spAssetName)))

  def gParamsPanel(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 250, 425), 3, Material.ImageEffects(AssetName(pmAssetName)))

  def gTimeSliderActiveBody(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 150, 1220), 3, Material.ImageEffects(AssetName(tsAssetName)))
      .withCrop(0, 25, 50, 1195)
      .scaleBy(sf, sf)

  def gTimeSliderActiveTop(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 150, 1220), 3, Material.ImageEffects(AssetName(tsAssetName)))
      .withCrop(0, 0, 50, 25)
      .scaleBy(sf, sf)

  def gTimeSliderInactiveBody(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 150, 1220), 3, Material.ImageEffects(AssetName(tsAssetName)))
      .withCrop(100, 25, 50, 1195)
      .scaleBy(sf, sf)

  def gTimeSliderInactiveTop(sf: Double): Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 150, 1220), 3, Material.ImageEffects(AssetName(tsAssetName)))
      .withCrop(100, 0, 50, 25)
      .scaleBy(sf, sf)

  // Check FlacFont.txt for details

  def buttonSplashAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 0, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 80, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(0, 160, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonSplashAssets

  def buttonParamsAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 0, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 80, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 160, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonParamsAssets

  def buttonPlayAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 0, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 80, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(480, 160, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonPlayAssets

  def buttonResultsAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 0, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 80, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(720, 160, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonResultsAssets

  def buttonRulesAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 240, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 320, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.ImageEffects(AssetName(bnAssetName))).withCrop(0, 400, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonRulesAssets

  def buttonNewGameAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 240, 240, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 320, 240, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(240, 400, 240, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonNewGameAssets

  def buttonPlusAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 0, 90, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 80, 90, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 160, 90, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonPlusAssets

  def buttonMinusAssets(sf: Double): ButtonAssets =
    val up =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 240, 90, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 320, 90, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(960, 400, 90, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonMinusAssets

  def buttonTurnAssets(sf: Double): ButtonAssets =
    val up = Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 0, 90, 80).scaleBy(sf, sf)
    val over =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 80, 90, 80).scaleBy(sf, sf)
    val down =
      Graphic(0, 0, 90, 80, 6, Material.Bitmap(AssetName(bnAssetName))).withCrop(1050, 160, 90, 80).scaleBy(sf, sf)
    ButtonAssets(up, over, down)
  end buttonTurnAssets

  def cornerTopLeft: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerTopRight: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomLeft: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomRight: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerLayers(r: Rectangle, sf: Double, cornerColor: RGBA): Batch[Graphic[ImageEffects]] =
    val newWidth20 = ((r.width - 20).toDouble * sf).toInt
    val newHeight20 = ((r.height - 20).toDouble * sf).toInt
    val layerC1 = (GameAssets.cornerTopLeft)
      .moveTo(r.left, r.top)
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
