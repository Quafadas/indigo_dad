package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode
import game.Piece.pieceShape

final case class FlicFlacGameModel(
    ourName: String, // ................. Subject to negotiation during startup
    oppoName: String, // ................ Subject to negotiation during startup
    ourPieceType: Int, // ............... Subject to negotiation during startup
    winningScore: Int, // ............... Subject to negotiation during startup
    randEventFreq: Int, // .............. Subject to negotiation during startup
    initiatorGameState: GameState, // ... Subject to negotiation during startup
    responderGameState: GameState, // ... Subject to negotiation during startup
    gameState: GameState, // ............ Updates
    gameScore: (Int, Int), // ........... Updates
    pieces: Pieces, // .................. Updates
    possibleMoveSpots: Spots, // ........ Updates
    highLighter: HighLighter, // ........ Updates
    turnTimer: TurnTimer, // ............ Updates
    hexBoard3: HexBoard3, // ............ FIXME No change by model so get it out
    scalingFactor: Double // ............ FIXME No change by model so get it out
) derives Encoder.AsObject,
      Decoder

enum GameState:
  case START_CON1 // a state included in the set return by getStartUpStates
  case START_CON2 // a state included in the set return by getStartUpStates
  case START_CON3 // a state included in the set return by getStartUpStates
  case START_CON4 // a state included in the set return by getStartUpStates
  case BLOCK_TURN
  case BLOCK_PAUSE
  case BLOCK_RESOLVE
  case CYLINDER_TURN
  case CYLINDER_PAUSE
  case CYLINDER_RESOLVE
  case FINISH
end GameState

enum PanelType:
  case P_INVISIBLE
  case P_ERROR
  case P_RESULTS
end PanelType

object FlicFlacGameModel:
  scribe.debug("@@@ Object FlicFlacGameModel Start")

  def creation(playerParams: FlicFlacPlayerParams): FlicFlacGameModel =
    scribe.debug("@@@ FlicFlacGameModel creation")

    val rand = new scala.util.Random
    val r = rand.nextInt(10)
    val pieceType = 
      if ( r>= 5) then
        // Initiator as BLOCK ... (as Responder our setting will be overridden)
        BLOCK
      else
        // Initiator as CYLINDER ... (as Responder our setting will be overridden)
        CYLINDER
      end if


    val sOurName = playerParams.playPams1_Name1
    val sOppoName = playerParams.playPams2_Name2
    val iOurPieceType = pieceType
    val iWinningScore = playerParams.playPams3_ScoreToWin
    val iRandEventFreq = playerParams.playPams6_RandEventProb
    val score = (0, 0)
    // pieces
    val startingSpots: Spots = Spots(Set.empty)
    // val highLighter = HighLighter(hexBoard3, false, Point(0, 0)) // moved to bottom as forward reference
    val turnTimer = TurnTimer(playerParams.playPams4_TurnTime, playerParams.playPams5_CaptorsTime)
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0, 0))
    val defaultScalingFactor = 1.0

    FlicFlacGameModel(
      sOurName,
      sOppoName,
      iOurPieceType,
      iWinningScore,
      iRandEventFreq,
      GameState.START_CON1,
      GameState.START_CON1,
      GameState.START_CON1,
      score,
      summonPieces(hexBoard3),
      startingSpots,
      highLighter,
      turnTimer,
      hexBoard3,
      defaultScalingFactor
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

  def modify(
      previousModel: FlicFlacGameModel,
      possiblePiece: Option[Piece],
      possibleHighLighter: Option[HighLighter]
  ): Outcome[FlicFlacGameModel] =
    val m1 = modifyPiece(previousModel, possiblePiece)
    val m2 = modifyHighLighter(m1, possibleHighLighter)
    val m3 = modifyPossibleMoves(m2)
    val asJson = m3.asJson.noSpaces
    val gameCache = getGameName(previousModel.ourName, previousModel.oppoName)
    org.scalajs.dom.window.localStorage.setItem(gameCache, asJson)
    Outcome(m3).addGlobalEvents(WebRtcEvent.SendData(m3))
  end modify

  def modifyPiece(previousModel: FlicFlacGameModel, possiblePiece: Option[Piece]): FlicFlacGameModel =
    possiblePiece match
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
  end modifyPiece

  def modifyPieces(previousModel: FlicFlacGameModel, newPieces: Pieces): Outcome[FlicFlacGameModel] =
    Outcome(previousModel.copy(pieces = newPieces))
  end modifyPieces

  def modifyHighLighter(previousModel: FlicFlacGameModel, possibleHighLighter: Option[HighLighter]): FlicFlacGameModel =
    possibleHighLighter match
      case Some(newHighLighter) =>
        previousModel.copy(highLighter = newHighLighter)
      case None =>
        previousModel
  end modifyHighLighter

  def modifyPossibleMoves(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    val newSpots = previousModel.possibleMoveSpots.calculatePossibleMoves(previousModel)
    previousModel.copy(possibleMoveSpots = newSpots)
  end modifyPossibleMoves

  def reset(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    scribe.debug("@@@ Reset model")

    val sOurName = previousModel.ourName
    val sOppoName = previousModel.oppoName
    val iOurPieceType = previousModel.ourPieceType
    val iWinningScore = previousModel.winningScore
    val iRandEventFreq = previousModel.randEventFreq
    val score = (0, 0)
    val defaultSF = 1.0
    val hexBoard3 = HexBoard3()
    val highLighter = HighLighter(hexBoard3, false, Point(0, 0))
    val emptySpots: Spots = Spots(Set.empty)
    val turnTime = previousModel.turnTimer.iTotalTurnTime
    val captorsTime = previousModel.turnTimer.iCaptorsTurnTime
    val turnTimer = TurnTimer(turnTime, captorsTime)
    FlicFlacGameModel(
      sOurName,
      sOppoName,
      iOurPieceType,
      iWinningScore,
      iRandEventFreq,
      GameState.START_CON1,
      GameState.START_CON1,
      GameState.START_CON1,
      score,
      summonPieces(hexBoard3),
      emptySpots,
      highLighter,
      turnTimer,
      hexBoard3,
      defaultSF
    )
  end reset

  def getGameName(ourName: String, oppoName: String): String =
    val sName: String = 
      if (ourName.compare(oppoName) < 0) then
        // we are the PeerJS initiator
        "FlicFlac-Game1"
      else
        // we are the PeerJS responder
        "FlicFlac-Game2"
      end if
    scribe.debug("@@@ getGameName: " + sName)
    sName
  end getGameName

  def getStartUpStates() : Set[GameState] = 
    val startUpStateSet = Set(
        GameState.START_CON1, 
        GameState.START_CON2, 
        GameState.START_CON3, 
        GameState.START_CON4
        )
    startUpStateSet
  end  getStartUpStates

  def retrieve(startupData: FlicFlacStartupData): FlicFlacGameModel =
    val playerParams = FlicFlacPlayerParams.getParams(startupData)
    val ourName = playerParams.playPams1_Name1
    val oppoName = playerParams.playPams2_Name2

    val gameCache = getGameName(ourName, oppoName) 
    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem(gameCache)) match
      case Right(model: FlicFlacGameModel) =>
        // FIXME we should check for version number here and goto create if mismatch
        scribe.debug("@@@ Restored model")
        FlicFlacGameModel.creation(playerParams)
      case Left(_) =>
        scribe.debug("@@@ Created model")
        FlicFlacGameModel.creation(playerParams)
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
