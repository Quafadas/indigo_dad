package game

import indigo.*

final case class Melee(model: FlicFlacGameModel):

  def step1() : Unit = 
    val cylinders = Set(
      model.pieces.modelPieces(0),
      model.pieces.modelPieces(1),
      model.pieces.modelPieces(2),
      model.pieces.modelPieces(3),
      model.pieces.modelPieces(4),
      model.pieces.modelPieces(5)
    )
    val blocks = Set(
      model.pieces.modelPieces(6),
      model.pieces.modelPieces(7),
      model.pieces.modelPieces(8),
      model.pieces.modelPieces(9),
      model.pieces.modelPieces(10),
      model.pieces.modelPieces(11)
    )

    var setConflicts = Set.empty[(Piece, Piece)]

    model.pieces.modelPieces.foreach { case (p) =>
      val qrs = model.hexBoard3.getQRSfromAxAy(p.pCurPos.x, p.pCurPos.y)
      val setQRS = model.possibleMoveSpots.spotRingQRS(qrs._1,qrs._2,qrs._3)
      if (p.pieceShape == CYLINDER) then
        blocks.foreach { case (b) => 
          val blk = model.hexBoard3.getQRSfromAxAy(b.pCurPos.x, b.pCurPos.y)
          if setQRS.contains(blk) then 
            setConflicts = setConflicts + ((p, b))
        }
      else
        cylinders.foreach { case (c) => 
          val cyl = model.hexBoard3.getQRSfromAxAy(c.pCurPos.x, c.pCurPos.y)
          if setQRS.contains(cyl) then 
            setConflicts = setConflicts + ((c, p))
        }

      end if
    }
    scribe.debug("@@@ ----")
    setConflicts.foreach { case (c,b) =>
        val pC = PieceAssets.pieceNames(c.pieceIdentity)
        val pB = PieceAssets.pieceNames(b.pieceIdentity)
        scribe.debug("@@@ Cylinder " + pC + " <=> Block " + pB)
      }
    scribe.debug("@@@ ----")

  end step1

object Melee {}

end Melee
