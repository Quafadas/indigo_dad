package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode
import game.Piece.pieceShape

final case class FlicFlacGameModel(
    gameState : GameState,
    gameScore : (Int, Int),
    turnTimeRemaining : Int,
    scalingFactor : Double,
    pieces: Pieces,
    possibleMoveSpots: Spots,
    highLighter: HighLighter,
    hexBoard3: HexBoard3
) derives Encoder.AsObject,
      Decoder

enum GameState {
  case START
  case BLOCK_TURN
  case BLOCK_PAUSE
  case BLOCK_RESOLVE
  case CYLINDER_TURN
  case CYLINDER_PAUSE
  case CYLINDER_RESOLVE
  case FINISH
}

object FlicFlacGameModel:
  scribe.debug("@@@ Object FlicFlacGameModel Start")
  var iTick = 0

  def creation(center: Point): FlicFlacGameModel =
    scribe.debug("@@@ FlicFlacGameModel creation")

    val startingGameState = GameState.CYLINDER_TURN   // FIXME
    val score = (0,0)
    val startingTurnTime = 90  // 10ths of a second FIXME we need this configurable
    val defaultScalingFactor = 1.0
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0,0))
    val startingSpots : Spots = Spots(Set.empty)
    FlicFlacGameModel(  
                        startingGameState,
                        score,
                        startingTurnTime,
                        defaultScalingFactor, 
                        summonPieces(hexBoard3), 
                        startingSpots,
                        highLighter, 
                        hexBoard3
                        )
  end creation

  def summonPieces(hexBoard3: HexBoard3): Pieces =
    val cy1 = hexBoard3.getCylinderHomePos(CB)
    val cy2 = hexBoard3.getCylinderHomePos(CG)
    val cy3 = hexBoard3.getCylinderHomePos(CY)
    val cy4 = hexBoard3.getCylinderHomePos(CO)
    val cy5 = hexBoard3.getCylinderHomePos(CR)
    val cy6 = hexBoard3.getCylinderHomePos(CP)
    val bk1 = hexBoard3.getBlockHomePos(CB)
    val bk2 = hexBoard3.getBlockHomePos(CG)
    val bk3 = hexBoard3.getBlockHomePos(CY)
    val bk4 = hexBoard3.getBlockHomePos(CO)
    val bk5 = hexBoard3.getBlockHomePos(CR)
    val bk6 = hexBoard3.getBlockHomePos(CP)

    val startingModelPieces: Vector[Piece] = Vector(
      Piece(CYLINDER, CB, cy1, cy1, cy1, false),
      Piece(CYLINDER, CG, cy2, cy2, cy2, false),
      Piece(CYLINDER, CY, cy3, cy3, cy3, false),
      Piece(CYLINDER, CO, cy4, cy4, cy4, false),
      Piece(CYLINDER, CR, cy5, cy5, cy5, false),
      Piece(CYLINDER, CP, cy6, cy6, cy6, false),
      Piece(BLOCK, CB, bk1, bk1, bk1, false),
      Piece(BLOCK, CG, bk2, bk2, bk2, false),
      Piece(BLOCK, CY, bk3, bk3, bk3, false),
      Piece(BLOCK, CO, bk4, bk4, bk4, false),
      Piece(BLOCK, CR, bk5, bk5, bk5, false),
      Piece(BLOCK, CP, bk6, bk6, bk6, false)
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
    val m1 = modifyPiece(previousModel, possiblePiece)
    val m2 = modifyHighLighter(m1, possibleHighLighter)
    val m3 = modifyPossibleMoves(m2)
    val asJson = m3.asJson.noSpaces
    org.scalajs.dom.window.localStorage.setItem("FlicFlacStats", asJson)
    m3
  end modify

  def modifyPiece(previousModel: FlicFlacGameModel, possiblePiece: Option[Piece]) : FlicFlacGameModel = 
    possiblePiece match {
      case Some(newPiece) =>
        var resultingVector: Vector[Piece] = Vector.empty
        for oldPiece <- previousModel.pieces.modelPieces do
          if (oldPiece.pieceShape == newPiece.pieceShape) && (oldPiece.pieceIdentity == newPiece.pieceIdentity) then
              resultingVector = resultingVector :+ newPiece
          else resultingVector = resultingVector :+ oldPiece
          end if
        end for
        val resultingPieces: Pieces = Pieces(resultingVector)
        previousModel.copy(pieces = resultingPieces)

      case None => 
        previousModel
    }
  end modifyPiece

  def modifyPieces(previousModel: FlicFlacGameModel, newPieces: Pieces) :  FlicFlacGameModel =
    previousModel.copy(pieces = newPieces)
  end modifyPieces


  def modifyHighLighter(previousModel : FlicFlacGameModel, possibleHighLighter: Option[HighLighter]) : FlicFlacGameModel = 
    possibleHighLighter match {
      case Some(newHighLighter) =>
        previousModel.copy(highLighter = newHighLighter)
      case None =>
        previousModel
    }
  end modifyHighLighter

  def modifyPossibleMoves(previousModel : FlicFlacGameModel) : FlicFlacGameModel = 
    val newSpots = previousModel.possibleMoveSpots.calculatePossibleMoves(previousModel)
    previousModel.copy(possibleMoveSpots = newSpots)
  end modifyPossibleMoves


  def reset(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    scribe.debug("@@@ Reset model")
    
    val resetGameState = GameState.CYLINDER_TURN
    val score = (0,0)
    val resetTurnTime = 0
    val defaultSF = 1.0
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0,0))
    val emptySpots : Spots = Spots(Set.empty)
    FlicFlacGameModel(  
                        resetGameState,
                        score,
                        resetTurnTime,
                        defaultSF,
                        summonPieces(hexBoard3),
                        emptySpots, 
                        highLighter, 
                        hexBoard3
                        )
  end reset

  def retrieve(): FlicFlacGameModel =

    // FIXME reading PlayerParams here is just a test
    val playerParams = FlicFlacPlayerParams.retrieve()
    scribe.debug("@@@ ScoreToWin: " + playerParams.playPamsScoreToWin)

    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem("FlicFlacStats")) match
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
      scribe.trace(s)
    end for
  end printPieces

  scribe.debug("@@@ Object FlicFlacGameModel Finish")
end FlicFlacGameModel
