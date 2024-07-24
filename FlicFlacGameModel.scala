package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode
import game.Piece.pieceShape

final case class FlicFlacGameModel(
    scalingFactor : Double,
    pieces: Pieces,
    highLighter: HighLighter,
    hexBoard3: HexBoard3
) derives Encoder.AsObject,
      Decoder

object FlicFlacGameModel:
  scribe.debug("@@@ Object FlicFlacGameModel Start")
  var iTick = 0

  def creation(center: Point): FlicFlacGameModel =
    scribe.debug("@@@ FlicFlacGameModel creation")
    val defaultScalingFactor = 1.0
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0,0))
    FlicFlacGameModel(defaultScalingFactor, summonPieces(hexBoard3), highLighter, hexBoard3)
  end creation

  def summonPieces(hexBoard3: HexBoard3): Pieces =
    val startingModelPieces: Vector[Piece] = Vector(
      Piece(CYLINDER, CB, hexBoard3.getCylinderHomePos(CB), hexBoard3.getCylinderHomePos(CB)),
      Piece(CYLINDER, CG, hexBoard3.getCylinderHomePos(CG), hexBoard3.getCylinderHomePos(CG)),
      Piece(CYLINDER, CY, hexBoard3.getCylinderHomePos(CY), hexBoard3.getCylinderHomePos(CY)),
      Piece(CYLINDER, CO, hexBoard3.getCylinderHomePos(CO), hexBoard3.getCylinderHomePos(CO)),
      Piece(CYLINDER, CR, hexBoard3.getCylinderHomePos(CR), hexBoard3.getCylinderHomePos(CR)),
      Piece(CYLINDER, CP, hexBoard3.getCylinderHomePos(CP), hexBoard3.getCylinderHomePos(CP)),
      Piece(BLOCK, CB, hexBoard3.getBlockHomePos(CB), hexBoard3.getBlockHomePos(CB)),
      Piece(BLOCK, CG, hexBoard3.getBlockHomePos(CG), hexBoard3.getBlockHomePos(CG)),
      Piece(BLOCK, CY, hexBoard3.getBlockHomePos(CY), hexBoard3.getBlockHomePos(CY)),
      Piece(BLOCK, CO, hexBoard3.getBlockHomePos(CO), hexBoard3.getBlockHomePos(CO)),
      Piece(BLOCK, CR, hexBoard3.getBlockHomePos(CR), hexBoard3.getBlockHomePos(CR)),
      Piece(BLOCK, CP, hexBoard3.getBlockHomePos(CP), hexBoard3.getBlockHomePos(CP))
    )
    Pieces(startingModelPieces)
  end summonPieces

  def findPieceByPos(model: FlicFlacGameModel, pos: Point): Option[Piece] =
    model.pieces.modelPieces.find(Piece.position(_) == pos)
  end findPieceByPos

  def findPieceSelected(model: FlicFlacGameModel): Option[Piece] =
    model.pieces.modelPieces.find(Piece.selected(_) == true)
  end findPieceSelected

  def modify(previousModel: FlicFlacGameModel, 
              possiblePiece: Option[Piece],
              possibleHighLighter: Option[HighLighter]): FlicFlacGameModel =

    (possiblePiece, possibleHighLighter) match
      case (Some(newPiece), Some(newHighLighter)) =>
        val interimModel = modifyPiece(previousModel, newPiece)
        val newModel = modifyHighLighter(interimModel, newHighLighter)
        val asJson = newModel.asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem("FlicFlac", asJson)
        newModel
      case (Some(newPiece), None) =>
        val newModel = modifyPiece(previousModel, newPiece)
        val asJson = newModel.asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem("FlicFlac", asJson)
        newModel
      case (None, Some(newHighLighter)) =>
        val newModel = modifyHighLighter(previousModel, newHighLighter)
        val asJson = newModel.asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem("FlicFlac", asJson)
        newModel
      case (None, None) =>
        previousModel
  end modify

  def modifyPiece(previousModel: FlicFlacGameModel, newPiece: Piece) : FlicFlacGameModel = 

    var resultingVector: Vector[Piece] = Vector.empty
    for oldPiece <- previousModel.pieces.modelPieces do
      if (oldPiece.pieceShape == newPiece.pieceShape) && (oldPiece.pieceIdentity == newPiece.pieceIdentity) then
          resultingVector = resultingVector :+ newPiece
      else resultingVector = resultingVector :+ oldPiece
      end if
    end for
    var resultingPieces: Pieces = Pieces(resultingVector)
    
    val previousSF = previousModel.scalingFactor
    val prevHighLighter = previousModel.highLighter
    val prevBoard = previousModel.hexBoard3
    val newModel = FlicFlacGameModel(previousSF, resultingPieces, prevHighLighter, prevBoard)
    printPieces(newModel)
    newModel

  end modifyPiece

  def modifyHighLighter(previousModel: FlicFlacGameModel, highLighter: HighLighter) : FlicFlacGameModel = 
    val previousSF = previousModel.scalingFactor
    val previousPieces = previousModel.pieces
    val prevBoard = previousModel.hexBoard3
    val newModel = FlicFlacGameModel(previousSF, previousPieces, highLighter, prevBoard)
    newModel       
  end modifyHighLighter

  def reset(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    scribe.debug("@@@ Reset model")
    val defaultSF = 1.0
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0,0))
    FlicFlacGameModel(defaultSF, summonPieces(hexBoard3),highLighter, hexBoard3)
  end reset

  def retrieve(): FlicFlacGameModel =
    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem("FlicFlac")) match
      case Right(model: FlicFlacGameModel) =>
        // FIXME we should check for version number here and goto create if mismatch
        scribe.debug("@@@ Restored model")
        model
      case Left(_) =>
        scribe.debug("@@@ Created model")
        FlicFlacGameModel.creation(Point(0, 0))
    cacheOrNew
  end retrieve

  def printPieces(model: FlicFlacGameModel): Unit =
    for p <- model.pieces.modelPieces do

      val sSelected = if Piece.selected(p) then "S" else "-"
      val sFlipped = if Piece.flipped(p) then "F" else "-"
      val sCaptured = if Piece.captured(p) then "C" else "-"
      val sMoved = if Piece.moved(p) then "M" else "-"

      val s = "@@@ " + PieceAssets.pieceTypes(p.pieceShape)
        + " " + PieceAssets.pieceNames(p.pieceIdentity % 6)
        + ": "
        + "CurPos(" + p.pCurPos.x + "," + p.pCurPos.y + ") "
        + "HomePos(" + p.pHomePos.x + "," + p.pHomePos.y + ") "
        + sSelected
        + sFlipped
        + sCaptured
        + sMoved
      scribe.debug(s)
    end for
  end printPieces

  scribe.debug("@@@ Object FlicFlacGameModel Finish")
end FlicFlacGameModel
