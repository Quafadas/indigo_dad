package game

import indigo.*

// 28/07/24 Tried to use Point instead of(Int,Int) but encoder/decoder throws compiler errors
final case class Spots( 
  indices: Set[(Int,Int)]
):
  scribe.debug("@@@ case class Spots Start")

  def calculatePossibleMoves(model: FlicFlacGameModel) : Spots = 
    scribe.debug("@@@ case Spots calculatePossibleMoves")

    val resultingSpots : Spots = Spots(Set.empty)
    FlicFlacGameModel.findPieceSelected(model) match
      case Some(piece) => 
        scribe.debug("@@@ Spots finds a selected piece")
        calculateSpots(model, piece)
        
      case None =>
        scribe.debug("@@@ Spots does not find a selected piece")
        Spots(Set.empty)
  end calculatePossibleMoves

  def calculateSpots(model: FlicFlacGameModel, piece: Piece) : Spots =
    val bBlocks = ((piece.pieceShape == BLOCK) && (model.gameState == GameState.BLOCK_TURN))
    val bCylinders = ((piece.pieceShape == CYLINDER) && (model.gameState == GameState.CYLINDER_TURN))
    if (bBlocks || bCylinders) then
      if Piece.moved(piece) then 
        scribe.debug("@@@ calculateSpots piece moved")
        Spots(Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y)))  // starting position only
      else
        scribe.debug("@@@ calculateSpots piece not moved @ " + piece.pCurPos)
        spotify(model: FlicFlacGameModel, piece: Piece)
      end if
    else
      scribe.debug("@@@ calculateSpots out of turn " + model.gameState )
      Spots(Set.empty)
    end if
  end calculateSpots

  def spotify (model: FlicFlacGameModel, piece: Piece) : Spots =
    val s0 : Spots =  Spots(Set.empty)


    // FIX HOME POSITION Set

    // Inner Ring
    // (0,-1,1) (1,-1,-0) (1, 0, -1) (0, 1, -1) (-1, 1, -0) (-1, 0, 1)

    val ax = piece.pCurPos.x
    val ay = piece.pCurPos.y
    val qrs = model.hexBoard3.getQRSfromAxAy(ax,ay)
    val q = qrs._1
    val r = qrs._2
    val s = qrs._3

    val ss1 = Set(
              (q+0,r-1,s+1),
              (q+1,r-1,s+0),
              (q+1,r+0,s-1),
              (q+0,r+1,s-1),
              (q-1,r+1,s+0),
              (q-1,r+0,s+1))

    var ss2 = Set((ax,ay))

    ss1.foreach{ case (qq,rr,ss) =>  
      val entry = model.hexBoard3.getAxAyfromQRS(qq,rr,ss)
      ss2 = ss2 + entry
      }

    val ss3 = Spots(ss2)
    ss3

    // Middle Ring

    // Outer Ring


  end spotify


  def paint(model: FlicFlacGameModel) : SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    val dSF = model.scalingFactor
    val pb = model.hexBoard3.pBase
    val layer = GameAssets.gSpot(dSF)

    for (pos <- model.possibleMoveSpots.indices) {
      val pPos = model.hexBoard3.getXpYp(Point(pos._1,pos._2))
      val spotLayer = Layer(layer.moveTo(model.hexBoard3.pBase.x + pPos.x, model.hexBoard3.pBase.y + pPos.y))
      val newFrag = SceneUpdateFragment(spotLayer)
      frag = frag |+| newFrag
    }
    frag

  scribe.debug("@@@ case class Spots Finish")
end Spots


