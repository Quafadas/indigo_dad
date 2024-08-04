package game

import indigo.*
import cats.instances.double
import game.PieceAssets.pieceNames

final case class Melee(model: FlicFlacGameModel):

  def combat() : Unit = 

    // Step 1 ... get a set of all conflicts by determining which blocks are next to each cylinder

    val cylinders = List(
      model.pieces.modelPieces(0),
      model.pieces.modelPieces(1),
      model.pieces.modelPieces(2),
      model.pieces.modelPieces(3),
      model.pieces.modelPieces(4),
      model.pieces.modelPieces(5)
    )

    val blocks = List(
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

    // Step 3 ... form a list of meleePieces 
    scribe.debug("@@@ === STEP 3")

    scribe.debug("@@@ All Melee Pieces:")
    var allMeleePieces = Set.empty[Piece]
    cylinderConflicts.foreach { case setConflicts =>
      setConflicts.foreach { case (c, b) =>
        allMeleePieces = allMeleePieces + c
        allMeleePieces = allMeleePieces + b
      }
    }

    allMeleePieces.foreach { p =>
      val shape = PieceAssets.pieceTypes(p.pieceShape)
      val color = PieceAssets.pieceNames(p.pieceIdentity)
      scribe.debug("@@@ " + shape + ":" + color)
    }
    scribe.debug("@@@ ====")

    scribe.debug("@@@ MeleeSets:")
    var allMeleeSets = List.empty[Set[(Piece)]]
    var processedPieces = Set.empty[Piece]
    cylinders.foreach { case cy =>
      if processedPieces.contains(cy) == false then
        val meleeSet = meleeBuilder(Set.empty[Piece], Set(cy), allConflicts)._2
        if meleeSet.isEmpty == false then
          allMeleeSets = allMeleeSets :+ meleeSet
        end if
        processedPieces = processedPieces.union(meleeSet)
      end if
    }

    allMeleeSets.foreach { case meleeSet =>
      meleeSet.foreach { case p =>
        val shape = PieceAssets.pieceTypes(p.pieceShape)
        val color = PieceAssets.pieceNames(p.pieceIdentity)
        scribe.debug("@@@ " + shape + ":" + color)
      }
      scribe.debug("@@@ **")
    }
    scribe.debug("@@@ ****")

    val listAllPieces = cylinders ++ blocks
    // Step 4 ... generate heath points according to flying colors, empowered and conflicts

    listAllPieces.foreach {case defendersPiece =>
        if allMeleePieces.contains(defendersPiece) then
          var iHealth = 0
          val justConflicts = 
            if defendersPiece.pieceShape == CYLINDER then
              cylinderConflicts.flatMap(set => set)
            else
              reversedBlockConflicts.flatMap(set => set)
            end if

          val activeConflicts = justConflicts.filter((d,a) => d == defendersPiece)
          activeConflicts.foreach { case (d,a) =>
            // d is defender
            // a is attacker
            val attackersBodyColor = a.pieceIdentity
            val attackersFlyingColor1 = if a.bFlipped then (a.pieceIdentity + 4) % 6 else (a.pieceIdentity + 1) % 6
            val attackersFlyingColor2 = if a.bFlipped then (a.pieceIdentity + 5) % 6 else (a.pieceIdentity + 2) % 6
            val defendersBodyColor = d.pieceIdentity
            val defendersFlyingColor1 = if d.bFlipped then (d.pieceIdentity + 4) % 6 else (d.pieceIdentity + 1) % 6
            val defendersFlyingColor2 = if d.bFlipped then (d.pieceIdentity + 5) % 6 else (d.pieceIdentity + 2) % 6

            // Evaluating Attack calculate FlyingColors, Empowered and Outnumbered
            if attackersFlyingColor1 == defendersBodyColor
            || attackersFlyingColor2 == defendersBodyColor then
              // the attacking piece is flying the correct colors so incurr damage              
              if model.hexBoard3.getHexColor(a.pCurPos) == attackersBodyColor then
                scribe.debug("@@@ Attacked - 2")
                iHealth = iHealth - 2 // the attacker is empowered
              else
                scribe.debug("@@@ Attacked - 1")
                iHealth = iHealth - 1 // the attacker is non-empowered
              end if

              allMeleeSets.find(meleeSet => meleeSet.contains(a)) match
                case Some(meleeSet) =>
                  //scribe.debug("@@@ meleeSet: " + meleeSet)
                  val iAttackers = (meleeSet.filter(p => p.pieceShape == a.pieceShape)).size
                  val iDefenders = meleeSet.size - iAttackers
                  val iOutNumbered = if iAttackers > iDefenders then 
                    scribe.debug("@@@ Outnumbered A>D so -1")
                    iHealth = iHealth - 1

                case None => 
                  ; // should not happen as we know piece is inside allMeleePieces
            end if

            // Evaluating Defense calculate FlyingColors, Empowered and Outnumbered
            if defendersFlyingColor1 == attackersBodyColor
            || defendersFlyingColor2 == attackersBodyColor then 
              if model.hexBoard3.getHexColor(d.pCurPos) == defendersBodyColor then 
                scribe.debug("@@@ Defended + 2")
                iHealth = iHealth + 2 // the defender is empowered
              else
                scribe.debug("@@@ Defended + 1")
                iHealth = iHealth + 1 // the defender is non-empowered
              end if

              allMeleeSets.find(meleeSet => meleeSet.contains(d)) match
                case Some(meleeSet) =>
                  //scribe.debug("@@@ meleeSet: " + meleeSet)
                  val iDefenders = (meleeSet.filter(p => p.pieceShape == d.pieceShape)).size
                  val iAttackers = meleeSet.size - iDefenders
                  val iOutNumbered = if iDefenders > iAttackers then 
                    scribe.debug("@@@ Outnumbered D>A so +1")
                    iHealth = iHealth + 1

                case None => 
                  ; // should not happen as we know piece is inside allMeleePieces
            end if

            val shape = PieceAssets.pieceTypes(defendersPiece.pieceShape)
            val color = PieceAssets.pieceNames(defendersPiece.pieceIdentity)
            scribe.debug("@@@ " + shape + ":" + color + " Health:" + iHealth)
          }
        else 
          val shape = PieceAssets.pieceTypes(defendersPiece.pieceShape)
          val color = PieceAssets.pieceNames(defendersPiece.pieceIdentity)
          scribe.debug("@@@ " + shape + ":" + color + " Restful")
        end if
    }


    // Step 5 ... generate a damage point for each outnumbered piece
    scribe.debug("@@@ Step 5 ... Damage Points")
    listAllPieces.foreach {case targetPiece =>
        if allMeleePieces.contains(targetPiece) then
          allMeleeSets.find(meleeSet => meleeSet.contains(targetPiece)) match
            case Some(meleeSet) =>
              //scribe.debug("@@@ meleeSet: " + meleeSet)
              val iFriend = (meleeSet.filter(p => p.pieceShape == targetPiece.pieceShape)).size
              val iEnemy = meleeSet.size - iFriend
              val iDamage = if iEnemy > iFriend then 1 else 0
              val shape = PieceAssets.pieceTypes(targetPiece.pieceShape)
              val color = PieceAssets.pieceNames(targetPiece.pieceIdentity)
              scribe.trace("@@@ " + shape + ":" + color + " $$" + iDamage)

            case None => 
              ; // should not happen as we know piece is inside allMeleePieces

        end if
      }
    scribe.trace("@@@ *****")


    /*
    so far we have ...
    cylinders Set[Piece] ... (c)
    blocks Set[Piece] ... (b)
    allConflicts Set[Piece,Piece] ... (c,b)
    cylinderConflicts List[Set[Piece,Piece]] ... (c,b)
    blockConflicts List[Set[Piece,Piece]] ... (c,b)
    reversedBlockConflicts List[Set[Piece,Piece]] ... (b,c)
    allMeleePieces Set[Piece]
    allMeleeSets List[Set[Piece]]
    */


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

object Melee {}

end Melee
