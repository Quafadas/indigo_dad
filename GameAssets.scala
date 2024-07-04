package game

import indigo.*
import indigoextras.ui.*

object GameAssets: 

  val bgAssetName = "bg"
  val bgAssetPath = "assets/BackGroundWhite.png"
  val hxAssetName = "hex2"
  val hxAssetPath = "assets/Hex2.png"
  val fbAssetName = "fourButtons"
  val fbAssetPath = "assets/FourButtons.png"
  val cyAssetName = "cylinders"
  val cyAssetPath = "assets/Cylinders.png"
  val blAssetName = "blocks"
  val blAssetPath = "assets/Blocks.png"

  println("@@@ Object GameAssets START")
    
  def get(): Set[AssetType] =
    Set(
//      AssetType.Image(AssetName(bgAssetName), AssetPath(bgAssetPath)),
      AssetType.Image(AssetName(hxAssetName), AssetPath(hxAssetPath)),
      AssetType.Image(AssetName(fbAssetName), AssetPath(fbAssetPath)),
      AssetType.Image(AssetName(cyAssetName), AssetPath(cyAssetPath)),
      AssetType.Image(AssetName(blAssetName), AssetPath(blAssetPath))
    )

//  def bgGraphic: Graphic[Material.ImageEffects] =                               // FIXME we should not need this !!!
//    Graphic(0, 0, 256, 256, 1, Material.ImageEffects(AssetName(bgAssetName)))

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
      down = Graphic(0, 0, 240, 80, 6, Material.Bitmap(AssetName(fbAssetName))).withCrop(720, 1200, 240, 80)
    )
  println("@@@ Object GameAssets FINISH")



