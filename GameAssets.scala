package game

import indigo.*
import indigoextras.ui.*
import indigo.shared.materials
import indigo.shared.materials.Material.ImageEffects

object GameAssets: 

  val hxAssetName = "hex2"
  val hxAssetPath = "assets/Hex2.png"
  val fbAssetName = "fourButtons"
  val fbAssetPath = "assets/FourButtons.png"
  val cyAssetName = "cylinders"
  val cyAssetPath = "assets/Cylinders.png"
  val blAssetName = "blocks"
  val blAssetPath = "assets/Blocks.png"
  val spAssetName = "splashBg"
  val spAssetPath = "assets/FlicFlacV4.png"
  val cnAssetName = "corners"
  val cnAssetPath = "assets/Corners.png"


  println("@@@ Object GameAssets START")
    
  def get(): Set[AssetType] =
    Set(
      AssetType.Image(AssetName(hxAssetName), AssetPath(hxAssetPath)),
      AssetType.Image(AssetName(fbAssetName), AssetPath(fbAssetPath)),
      AssetType.Image(AssetName(cyAssetName), AssetPath(cyAssetPath)),
      AssetType.Image(AssetName(blAssetName), AssetPath(blAssetPath)),
      AssetType.Image(AssetName(spAssetName), AssetPath(spAssetPath)),
      AssetType.Image(AssetName(cnAssetName), AssetPath(cnAssetPath))      
    )

  def gHex: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 91, 81), 2, Material.ImageEffects(AssetName(hxAssetName)))


  def buttonSplashAssets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(0, 0, 240, 80),
      over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(0, 80, 240, 80),
      down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(0, 160, 240, 80)
    )

  def buttonParamsAssets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(240, 0, 240, 80),
      over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(240, 80, 240, 80),
      down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(240, 160, 240, 80)
    )

  def buttonGameAssets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(480, 0, 240, 80),
      over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(480, 80, 240, 80),
      down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(480, 160, 240, 80)
    )

  def buttonResultsAssets: ButtonAssets =
    ButtonAssets(
      up = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(720, 0, 240, 80),
      over = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(720, 80, 240, 80),
      down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(720, 160, 240, 80)
    )
  def buttonResultsDown: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 240, 80), 6, Material.ImageEffects(AssetName(fbAssetName))).withCrop(720, 160, 240, 80)

  def splashBg: Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 1920, 1080), 2, Material.ImageEffects(AssetName(spAssetName)))

  def cornerTopLeft : Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerTopRight : Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 0, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomLeft : Graphic[Material.ImageEffects] =
    Graphic(Rectangle(0, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerBottomRight : Graphic[Material.ImageEffects] =
    Graphic(Rectangle(30, 30, 20, 20), 2, Material.ImageEffects(AssetName(cnAssetName)))

  def cornerLayers(width: Int, height: Int, sf: Double, cornerColor: RGBA) : Batch[Graphic[ ImageEffects]] =
      val layerC1 = (GameAssets.cornerTopLeft).moveTo(0,0).modifyMaterial(_.withTint(cornerColor)).scaleBy(sf,sf)
      val layerC2 = (GameAssets.cornerTopRight).moveTo(width-20,0).modifyMaterial(_.withTint(cornerColor)).scaleBy(sf,sf)
      val layerC3 = (GameAssets.cornerBottomLeft).moveTo(0,height-20).modifyMaterial(_.withTint(cornerColor)).scaleBy(sf,sf)
      val layerC4 = (GameAssets.cornerBottomRight).moveTo(width-20,height-20).modifyMaterial(_.withTint(cornerColor)).scaleBy(sf,sf)
      Batch(layerC1,layerC2,layerC3,layerC4)    

  println("@@@ Object GameAssets FINISH")





