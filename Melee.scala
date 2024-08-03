package game

import indigo.*
import cats.instances.double

final case class Melee(model: FlicFlacGameModel):

  def combat() : Unit = 

    // Step 1 ... get a set of all conflicts by determining which blocks are next to each cylinder

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
    scribe.debug("@@@ ---- STEP 1")
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

    scribe.debug("@@@ ==== STEP 2")
    cylinderConflicts.foreach { case (s) =>
      s.foreach { case (c,b) =>
          val shape1 = PieceAssets.pieceTypes(c.pieceShape)
          val color1 = PieceAssets.pieceNames(c.pieceIdentity)
          val shape2 = PieceAssets.pieceTypes(b.pieceShape)
          val color2 = PieceAssets.pieceNames(b.pieceIdentity)
          scribe.debug("@@@ " + shape1 +":" + color1 + " <=> " + shape2 +":" + color2)
        }
      scribe.debug("@@@ ==")
      }
    reversedBlockConflicts.foreach { case (s) =>
      s.foreach { case (c,b) =>
          val shape1 = PieceAssets.pieceTypes(c.pieceShape)
          val color1 = PieceAssets.pieceNames(c.pieceIdentity)
          val shape2 = PieceAssets.pieceTypes(b.pieceShape)
          val color2 = PieceAssets.pieceNames(b.pieceIdentity)
          scribe.debug("@@@ " + shape1 +":" + color1 + " <=> " + shape2 +":" + color2)
        }
      scribe.debug("@@@ ==")
    }
    scribe.debug("@@@ ====")

    // Step 3 ... form a list of melees 

    scribe.debug("@@@ MeleeSet:")
    cylinders.foreach { case cy =>  // FIXME this is not the way to iterate through the melees ???
      val meleeSet = meleeBuilder(Set.empty[Piece], Set(cy), allConflicts)._2
      meleeSet.foreach { case p =>
        val shape = PieceAssets.pieceTypes(p.pieceShape)
        val color = PieceAssets.pieceNames(p.pieceIdentity)
        scribe.debug("@@@ " + shape + ":" + color)
      }
      scribe.debug("@@@ ****")
    }


    val melee1 = List.empty[Set[(Piece, Piece)]]
    
  end combat

  def reverseConflictsList[A, B](list: List[Set[(A, B)]]): List[Set[(B, A)]] = {
    list.map { set =>
      set.map { case (a, b) =>
        (b, a)
      }
    }
  }
  end reverseConflictsList

  /* meleeBuilder
   * s1 starts as Set.empty[Piece] and changes through recursion
   * s2 starts as Set(TargetPiece) and changes through recursion
   * s9 is allConflicts and does not change
   */

  def meleeBuilder(s1: Set[Piece], s2: Set[Piece], s9: Set[(Piece, Piece)]) : (Set[Piece], Set[Piece]) =
    if s1.size == s2.size then 
      if s1.size <= 1 then
        val s0 = Set.empty[Piece]
        (s0, s0) // return empty because there are no other pieces in conflict
      else
        (s1, s2)
      end if 
    else
      var s3 = Set.empty[Piece] ++ s2 // the previous set
      var s4 = Set.empty[Piece] ++ s2 // the new set calculated below
      s2.foreach { case (p) =>
        val s5 = s9.filter((c,b) => p==c)
        val s6 = s5.map((c,b) => b)
        s4 = s4.union(s6)
      }
      s2.foreach { case (p) =>
        val s7 = s9.filter((c,b) => p==b)
        val s8 = s7.map((c,b) => c)
        s4 = s4.union(s8)
      }
      meleeBuilder(s3,s4,s9)
  end meleeBuilder

        // val conflicts = allConflicts.filter((ccc,bbb) => (ccc == c))

object Melee {}

end Melee
