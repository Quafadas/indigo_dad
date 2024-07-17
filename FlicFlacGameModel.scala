package game

import indigo.*

final case class FlicFlacGameModel(
  boardConfig : BoardConfig,
  modelPieces : Vector[Piece]
)

object FlicFlacGameModel:
  println("@@@ Object FlicFlacGameModel Start")
  var iTick = 0

  def creation(center: Point): FlicFlacGameModel = 
    println("@@@ FlicFlacGameModel creation")
    val skaleFaktor = 1.0                           // FIXME when scale strategy decided
    val bCfg = establishBoardConfig()              // FIXME drop the 2 when immutability finished
    val hexBoard = HexBoard(bCfg, skaleFaktor)    // FIXME drop the 2 when immutability finished
  
    
    FlicFlacGameModel(bCfg, summonPieces(hexBoard))

  def establishBoardConfig() : BoardConfig = 
    val boardCfg = BoardConfig(
      91, // .......................... GWIDTH pixel width of graphic
      81, // .......................... GHEIGHT pixel height of graphic
      Point(260,30), // ............... where the (inisible) top left hand corner of the hex grid board is positioned
      3, // ........................... game size
      70, // .......................... amount to add to a hex centre x coord to reach the vertical line of the next column
      40, // .......................... half the amount to add to a hex centre y coord to reach the next hexagon below
      10 // ........................... xcoord of halfway along the top left diagonal line of first hex
    )
    boardCfg
  end establishBoardConfig

  def summonPieces(hexBoard: HexBoard) : Vector[Piece] = 
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

  def modify(previousModel: FlicFlacGameModel, possiblePiece: Option[Piece]) : FlicFlacGameModel =    
    possiblePiece match
      case Some(newPiece) =>
        var resultingPieces: Vector[Piece] = Vector.empty
        for (oldPiece <- previousModel.modelPieces)
          if ((oldPiece.pieceShape == newPiece.pieceShape) && (oldPiece.pieceIdentity == newPiece.pieceIdentity))
            resultingPieces = resultingPieces :+ newPiece
          else 
            resultingPieces = resultingPieces :+ oldPiece
        val ffgm = FlicFlacGameModel(previousModel.boardConfig, resultingPieces)
        ffgm
        

      case None =>
        previousModel
      end match
      
  end modify

  
  def debugJP(id:String, iTickStart:Int, model:FlicFlacGameModel) : Unit = 
    if (iTickStart > 0)
      iTick = iTickStart
    if (iTick > 0) then 
      println("@@@ $$ " + id)
      println("@@@ " + model.modelPieces.head)
      iTick = iTick - 1
    end if 

  println("@@@ Object FlicFlacGameModel Finish")
