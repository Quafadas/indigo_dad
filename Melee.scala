package game

import indigo.*
import cats.instances.double

final case class Melee(model: FlicFlacGameModel):

  def combat() : Unit = 

    // Step 1 ... get a set of all conflicts by determining which blocks are next to each cylinder

    val blocks = Set(
      model.pieces.modelPieces(6),
      model.pieces.modelPieces(7),
      model.pieces.modelPieces(8),
      model.pieces.modelPieces(9),
      model.pieces.modelPieces(10),
      model.pieces.modelPieces(11)
    )

    var allConflicts = Set.empty[(Piece, Piece)]

    model.pieces.modelPieces.foreach { case (p) =>
      val qrs = model.hexBoard3.getQRSfromAxAy(p.pCurPos.x, p.pCurPos.y)
      val setQRS = model.possibleMoveSpots.spotRingQRS(qrs._1,qrs._2,qrs._3)
      if (p.pieceShape == CYLINDER) then
        blocks.foreach { case (b) => 
          val blk = model.hexBoard3.getQRSfromAxAy(b.pCurPos.x, b.pCurPos.y)
          if setQRS.contains(blk) then 
            allConflicts = allConflicts + ((p, b))
        }
      end if
    }
    scribe.debug("@@@ ----")
    allConflicts.foreach { case (c,b) =>
        val pC = PieceAssets.pieceNames(c.pieceIdentity)
        val pB = PieceAssets.pieceNames(b.pieceIdentity)
        scribe.debug("@@@ Cylinder " + pC + " <=> Block " + pB)
      }
    scribe.debug("@@@ ----")

    // Step 2 ... get a cylinder list and a block list for conflicts
    // ... each list to contain a set of conflicts for each piece in combat

    var cylinderConflicts = List.empty[Set[(Piece, Piece)]]

    allConflicts.foreach { case (c,b) =>
      val justConflicts = cylinderConflicts.flatMap(set => set)
      val cylinderConflictsRecorded = justConflicts.find {case (cc,bb) => cc == c}
      if cylinderConflictsRecorded == None then
        val conflicts = allConflicts.filter((ccc,bbb) => (ccc == c))
        cylinderConflicts = cylinderConflicts :+ conflicts
      end if
    }

    var blockConflicts = List.empty[Set[(Piece, Piece)]]

    allConflicts.foreach { case (c,b) =>
      val justConflicts = blockConflicts.flatMap(set => set)
      val blockConflictsRecorded = justConflicts.find { case (cc,bb) => bb == b }
      if blockConflictsRecorded == None then
        val conflicts = allConflicts.filter((ccc,bbb) => (bbb == b))
//        val reversedConflicts = conflicts.map(reversePiecePair)
        blockConflicts = blockConflicts :+ conflicts
      end if
    }

    val reversedBlockConflicts = reverseConflictsList(blockConflicts)

    scribe.debug("@@@ ====")
    cylinderConflicts.foreach { case (s) =>
      s.foreach { case (c,b) =>
          val shape1 = PieceAssets.pieceTypes(c.pieceShape)
          val color1 = PieceAssets.pieceNames(c.pieceIdentity)
          val shape2 = PieceAssets.pieceTypes(b.pieceShape)
          val color2 = PieceAssets.pieceNames(b.pieceIdentity)
          scribe.debug("@@@ " + shape1 +":" + color1 + " <=> " + shape2 +":" + color2)
        }
      }
    reversedBlockConflicts.foreach { case (s) =>
      s.foreach { case (c,b) =>
          val shape1 = PieceAssets.pieceTypes(c.pieceShape)
          val color1 = PieceAssets.pieceNames(c.pieceIdentity)
          val shape2 = PieceAssets.pieceTypes(b.pieceShape)
          val color2 = PieceAssets.pieceNames(b.pieceIdentity)
          scribe.debug("@@@ " + shape1 +":" + color1 + " <=> " + shape2 +":" + color2)
        }

    }
    scribe.debug("@@@ ====")

  end combat


  def reverseConflictsList[A, B](list: List[Set[(A, B)]]): List[Set[(B, A)]] = {
    list.map { set =>
      set.map { case (a, b) =>
        (b, a)
      }
    }
  }
  end reverseConflictsList


object Melee {}

end Melee
