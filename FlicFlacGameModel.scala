package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode
import game.Piece.pieceShape

final case class FlicFlacGameModel(
    boardConfig: BoardConfig,
    modelPieces: Vector[Piece]
) derives Encoder.AsObject,
      Decoder

object FlicFlacGameModel:
  println("@@@ Object FlicFlacGameModel Start")
  var iTick = 0

  def creation(center: Point): FlicFlacGameModel =
    println("@@@ FlicFlacGameModel creation")
    val skaleFaktor = 1.0 // FIXME when scale strategy decided
    val bCfg = establishBoardConfig()
    val hexBoard = HexBoard(bCfg, skaleFaktor)

    FlicFlacGameModel(bCfg, summonPieces(hexBoard))
  end creation

  def establishBoardConfig(): BoardConfig =
    val boardCfg = BoardConfig(
      91, // .......................... GWIDTH pixel width of graphic
      81, // .......................... GHEIGHT pixel height of graphic
      Point(260, 30), // ............... where the (inisible) top left hand corner of the hex grid board is positioned
      3, // ........................... game size
      70, // .......................... amount to add to a hex centre x coord to reach the vertical line of the next column
      40, // .......................... half the amount to add to a hex centre y coord to reach the next hexagon below
      10 // ........................... xcoord of halfway along the top left diagonal line of first hex
    )
    boardCfg
  end establishBoardConfig

  def summonPieces(hexBoard: HexBoard): Vector[Piece] =
    val startingModelPieces: Vector[Piece] = Vector(
      Piece(CYLINDER, CB, hexBoard.getCylinderHomePos(CB), hexBoard.getCylinderHomePos(CB)),
      Piece(CYLINDER, CG, hexBoard.getCylinderHomePos(CG), hexBoard.getCylinderHomePos(CG)),
      Piece(CYLINDER, CY, hexBoard.getCylinderHomePos(CY), hexBoard.getCylinderHomePos(CY)),
      Piece(CYLINDER, CO, hexBoard.getCylinderHomePos(CO), hexBoard.getCylinderHomePos(CO)),
      Piece(CYLINDER, CR, hexBoard.getCylinderHomePos(CR), hexBoard.getCylinderHomePos(CR)),
      Piece(CYLINDER, CP, hexBoard.getCylinderHomePos(CP), hexBoard.getCylinderHomePos(CP)),
      Piece(BLOCK, CB, hexBoard.getBlockHomePos(CB), hexBoard.getBlockHomePos(CB)),
      Piece(BLOCK, CG, hexBoard.getBlockHomePos(CG), hexBoard.getBlockHomePos(CG)),
      Piece(BLOCK, CY, hexBoard.getBlockHomePos(CY), hexBoard.getBlockHomePos(CY)),
      Piece(BLOCK, CO, hexBoard.getBlockHomePos(CO), hexBoard.getBlockHomePos(CO)),
      Piece(BLOCK, CR, hexBoard.getBlockHomePos(CR), hexBoard.getBlockHomePos(CR)),
      Piece(BLOCK, CP, hexBoard.getBlockHomePos(CP), hexBoard.getBlockHomePos(CP))
    )
    startingModelPieces
  end summonPieces

  def findPieceByPos(model: FlicFlacGameModel, pos: Point): Option[Piece] =
    model.modelPieces.find(Piece.position(_) == pos)
  end findPieceByPos

  def findPieceSelected(model: FlicFlacGameModel): Option[Piece] =
    model.modelPieces.find(Piece.selected(_) == true)
  end findPieceSelected

  def modify(previousModel: FlicFlacGameModel, possiblePiece: Option[Piece]): FlicFlacGameModel =
    possiblePiece match
      case Some(newPiece) =>
        var resultingPieces: Vector[Piece] = Vector.empty
        for oldPiece <- previousModel.modelPieces do
          if (oldPiece.pieceShape == newPiece.pieceShape) && (oldPiece.pieceIdentity == newPiece.pieceIdentity) then
            resultingPieces = resultingPieces :+ newPiece
          else resultingPieces = resultingPieces :+ oldPiece
        end for
        val newModel = FlicFlacGameModel(previousModel.boardConfig, resultingPieces)
        val asJson = newModel.asJson.noSpaces
        org.scalajs.dom.window.localStorage.setItem("FlicFlac", asJson)
        printPieces(newModel)
        newModel

      case None =>
        previousModel
    end match
  end modify

  def reset(previousModel: FlicFlacGameModel): FlicFlacGameModel =
    println("@@@ Reset model")
    val skaleFaktor = 1.0 // FIXME when scale strategy decided
    val previousBoardCfg = previousModel.boardConfig
    val hexBoard = HexBoard(previousBoardCfg, skaleFaktor)
    FlicFlacGameModel(previousBoardCfg, summonPieces(hexBoard))
  end reset

  def retrieve(): FlicFlacGameModel =
    val cacheOrNew = decode[FlicFlacGameModel](org.scalajs.dom.window.localStorage.getItem("FlicFlac")) match
      case Right(model: FlicFlacGameModel) =>
        println("@@@ Restored model")
        model
      case Left(_) =>
        println("@@@ Created model")
        FlicFlacGameModel.creation(Point(0, 0))
    cacheOrNew
  end retrieve

  def printPieces(model: FlicFlacGameModel): Unit =
    for p <- model.modelPieces do

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
      println(s)
    end for
  end printPieces
  /*
    pCurPos: Point, // ................. current position (in hexArrayCoords)
    pHomePos: Point, // ................ starting/home position (in hexArrayCoords)

    // parameters below required for model, but not for creation

    bFlipped: Boolean = false, // ...... piece normal is f, piece flipped is 1
    bSelected: Boolean = false, // ..... piece is selected
    bCaptured: Boolean = false, // ..... piece is captured (or not)
    bMoved: Boolean = false // ......... piece has moved this turn
   */

  def debugJP(id: String, iTickStart: Int, model: FlicFlacGameModel): Unit =
    if iTickStart > 0 then iTick = iTickStart
    end if
    if iTick > 0 then
      println("@@@ $$ " + id)
      println("@@@ " + model.modelPieces.head)
      iTick = iTick - 1
    end if
  end debugJP

  println("@@@ Object FlicFlacGameModel Finish")
end FlicFlacGameModel
