package game

import indigo.*

// 28/07/24 Tried to use Point instead of(Int,Int) but encoder/decoder throws compiler errors
final case class Spots(
    indices: Set[(Int, Int)]
):
  // scribe.debug("@@@ case class Spots Start")

  def calculatePossibleMoves(model: FlicFlacGameModel): Spots =
    scribe.debug("@@@ Spots calculatePossibleMoves")

    val resultingSpots: Spots = Spots(Set.empty)
    FlicFlacGameModel.findPieceSelected(model) match
      case Some(piece) =>
        scribe.debug("@@@ Spots finds a selected piece")
        calculateSpots(model, piece)

      case None =>
        scribe.debug("@@@ Spots does not find a selected piece")
        Spots(Set.empty)
    end match
  end calculatePossibleMoves

  def calculateSpots(model: FlicFlacGameModel, piece: Piece): Spots =
    scribe.debug("@@@ Spots calculateSpots start")
    val bBlocks = ((piece.pieceShape == BLOCK) && (model.gameState == GameState.BLOCK_TURN))
    val bCylinders = ((piece.pieceShape == CYLINDER) && (model.gameState == GameState.CYLINDER_TURN))
    if bBlocks || bCylinders then
      if Piece.moved(piece) then
        scribe.debug("@@@ calculateSpots piece moved")
        model.pieces.modelPieces.find(p => (p.pCurPos == piece.pTurnStartPos)) match
          case Some(p) =>
            Spots(Set.empty)
          case None =>
            Spots(Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y))) // starting position only
        end match
      else
        scribe.debug("@@@ calculateSpots piece not moved @ " + piece.pCurPos)
        spotify(model: FlicFlacGameModel, piece: Piece)
      end if
    else
      scribe.debug("@@@ calculateSpots out of turn " + model.gameState)
      Spots(Set.empty)
    end if
  end calculateSpots

  def spotify(model: FlicFlacGameModel, piece: Piece): Spots =

    val vPieces = model.pieces.modelPieces

    val ax = piece.pCurPos.x
    val ay = piece.pCurPos.y
    val qrs = hexBoard4.getQRSfromAxAy(ax, ay)
    val q = qrs._1
    val r = qrs._2
    val s = qrs._3

    if piece.pCurPos == piece.pHomePos then

      scribe.debug("@@@ Spotify TP1")

      // we have a piece in the home position so display unoccupied starting places
      var ss1 = Set.empty[(Int, Int)]

      piece.pieceShape match
        case CYLINDER =>
          if piece.pieceIdentity == CB || piece.pieceIdentity == CR || piece.pieceIdentity == CY then
            // spots for top left
            ss1 = Set((0, 9), (1, 8), (1, 7), (2, 6), (2, 5), (3, 4), (3, 3), (4, 2))
          else
            // spots for bottom right
            ss1 = Set((4, 30), (4, 29), (5, 28), (5, 27), (6, 26), (6, 25), (7, 24), (7, 23))
          end if
        case BLOCK =>
          if piece.pieceIdentity == CB || piece.pieceIdentity == CR || piece.pieceIdentity == CY then
            // spots for top right
            ss1 = Set((0, 23), (1, 24), (1, 25), (2, 26), (2, 27), (3, 28), (3, 29), (4, 30))
          else
            // spots for bottom left
            ss1 = Set((4, 2), (4, 3), (5, 4), (5, 5), (6, 6), (6, 7), (7, 8), (7, 9))
          end if
      end match

      val ss2 = ss1.filter { case (aX, aY) => hexBoard4.isThisHexFree(Point(aX, aY), vPieces) }
      scribe.debug("@@@ spotify Home free hex count: " + ss2.size)
      Spots(ss2)
    else if piece.bMoved then
      scribe.debug("@@@ Spotify TP2")
      // we have a piece that has already moved this turn
      val ss1 = Set((piece.pTurnStartPos.x, piece.pTurnStartPos.y))
      Spots(ss1)
    else
      scribe.debug("@@@ Spotify TP3")

      // we have a piece on the board trying to move so calculate valid moves from Ring1,Ring2,Ring3
      // Inner Ring

      val setInnerRing = spotRingQRS(q, r, s)
      var setInnerRingAxAy = Set.empty[(Int, Int)]
      var setInnerRingQRS = Set.empty[(Int, Int, Int)]

      setInnerRing.foreach { case (q1, r1, s1) =>
        val aX1aY1 = hexBoard4.getAxAyfromQRS(q1, r1, s1)
        val q1r1s1 = (q1, r1, s1)
        if hexBoard4.isThisHexValid(q1, r1, s1)
          && hexBoard4.isThisHexFree(q1, r1, s1, vPieces)
        then
          setInnerRingAxAy = setInnerRingAxAy + aX1aY1
          setInnerRingQRS = setInnerRingQRS + q1r1s1
        end if
      }
      scribe.debug("@@@ spotify Ring1 free hex count: " + setInnerRingQRS.size)

      // Middle Ring

      var setMiddleRingAxAy = Set.empty[(Int, Int)]
      var setMiddleRingQRS = Set.empty[(Int, Int, Int)]
      val setInnerRingNotBlackQRS =
        setInnerRingQRS.filter((q2f, r2f, s2f) => hexBoard4.isThisHexBlack(q2f, r2f, s2f) == false)
      setInnerRingNotBlackQRS.foreach { case (q2, r2, s2) =>
        val set2M = spotRingQRS(q2, r2, s2)
        set2M.foreach { case (q2m, r2m, s2m) =>
          if hexBoard4.isThisHexValid(q2m, r2m, s2m)
            && hexBoard4.isThisHexBlack(q2m, r2m, s2m) == false
            && hexBoard4.isThisHexFree(q2m, r2m, s2m, vPieces)
          then
            val aX2aY2 = hexBoard4.getAxAyfromQRS(q2m, r2m, s2m)
            val q2r2s2 = (q2m, r2m, s2m)
            setMiddleRingAxAy = setMiddleRingAxAy + aX2aY2
            setMiddleRingQRS = setMiddleRingQRS + q2r2s2
        }
      }

      // Outer Ring

      var setOuterRingAxAy = Set.empty[(Int, Int)]
      var setOuterRingQRS = Set.empty[(Int, Int, Int)]
      val setMiddleRingNotBlackQRS =
        setMiddleRingQRS.filter((q3f, r3f, s3f) => hexBoard4.isThisHexBlack(q3f, r3f, s3f) == false)
      setMiddleRingNotBlackQRS.foreach { case (q3, r3, s3) =>
        val set3M = spotRingQRS(q3, r3, s3)
        set3M.foreach { case (q3m, r3m, s3m) =>
          if hexBoard4.isThisHexValid(q3m, r3m, s3m)
            && hexBoard4.isThisHexBlack(q3m, r3m, s3m) == false
            && hexBoard4.isThisHexFree(q3m, r3m, s3m, vPieces)
          then
            val aX3aY3 = hexBoard4.getAxAyfromQRS(q3m, r3m, s3m)
            val q3r3s3 = (q3m, r3m, s3m)
            setOuterRingAxAy = setOuterRingAxAy + aX3aY3
            setOuterRingQRS = setOuterRingQRS + q3r3s3
        }
      }
      Spots(setInnerRingAxAy.union(setMiddleRingAxAy).union(setOuterRingAxAy))
    end if

  end spotify

  def spotRingQRS(q: Int, r: Int, s: Int): Set[(Int, Int, Int)] =
    // Inner Ring offsets are ...
    // (0,-1,1) (1,-1,-0) (1, 0, -1) (0, 1, -1) (-1, 1, -0) (-1, 0, 1)
    val ring1 = Set(
      (q + 0, r - 1, s + 1),
      (q + 1, r - 1, s + 0),
      (q + 1, r + 0, s - 1),
      (q + 0, r + 1, s - 1),
      (q - 1, r + 1, s + 0),
      (q - 1, r + 0, s + 1)
    )
    ring1
  end spotRingQRS

  def paint(model: FlicFlacGameModel): Layer =
    val dSF = hexBoard4.scalingFactor
    val pb = hexBoard4.pBase
    val layer = GameAssets.gSpot(dSF)
    var multiSpot = Layer.empty

    for pos <- model.possibleMoveSpots.indices do
      val pPos = hexBoard4.getXsYs(Point(pos._1, pos._2))
      val spotLayer = Layer(layer.moveTo(hexBoard4.pBase.x + pPos.x, hexBoard4.pBase.y + pPos.y))
      multiSpot = multiSpot |+| spotLayer
    end for
    multiSpot
  end paint

  // scribe.debug("@@@ case class Spots Finish")
end Spots
