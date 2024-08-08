package game

import indigo.*
import cats.instances.double
import game.PieceAssets.pieceNames

final case class Melee(model: FlicFlacGameModel):

  def combat(model: FlicFlacGameModel) : Pieces = 

    // Step A ... get 4 sets of links

    val allPieces = model.pieces.modelPieces
    var vectorHealth : Array[Int] = Array(0,0,0,0,0,0,0,0,0,0,0,0)
    
    val (cylinders, blocks) = allPieces splitAt 6 // Cylinders are 0...5 & blocks are 6...11

    var allPiecesQRS = Vector.empty[(Int,Int,Int)]    
    var allPiecesEmpowered = Vector.empty[(Boolean)]
    allPieces.foreach { piece =>
      val qrs = model.hexBoard3.getQRSfromAxAy(piece.pCurPos.x, piece.pCurPos.y)
      allPiecesQRS = allPiecesQRS :+ qrs
      if model.hexBoard3.getHexColor(piece.pCurPos) == piece.pieceIdentity then
        allPiecesEmpowered = allPiecesEmpowered :+ true
      else 
        allPiecesEmpowered = allPiecesEmpowered :+ false
      end if 
    }

    allPieces.foreach { p1 =>
      val index1 = (p1.pieceShape * 6) + p1.pieceIdentity
      val color1 = (PieceAssets.pieceNames(index1%6)).trim
      val qrs1 = allPiecesQRS(index1)
      val setQRS1 = model.possibleMoveSpots.spotRingQRS(qrs1._1,qrs1._2,qrs1._3)
      allPieces.foreach { p2 => 
        val index2 = (p2.pieceShape * 6) + p2.pieceIdentity
        val color2 = (PieceAssets.pieceNames(index2%6)).trim
        val qrs2 = allPiecesQRS(index2)
        if setQRS1.contains(qrs2) then
          val p1BodyColor = p1.pieceIdentity
          val p1FlyingColor1 = if p1.bFlipped then (p1.pieceIdentity + 4) % 6 else (p1.pieceIdentity + 1) % 6
          val p1FlyingColor2 = if p1.bFlipped then (p1.pieceIdentity + 5) % 6 else (p1.pieceIdentity + 2) % 6
          val p2BodyColor = p2.pieceIdentity
          val p2FlyingColor1 = if p2.bFlipped then (p2.pieceIdentity + 4) % 6 else (p2.pieceIdentity + 1) % 6
          val p2FlyingColor2 = if p2.bFlipped then (p2.pieceIdentity + 5) % 6 else (p2.pieceIdentity + 2) % 6

          if p1BodyColor == p2FlyingColor1
          || p1BodyColor == p2FlyingColor2 then 
            (p1.pieceShape, p2.pieceShape) match {
              case (CYLINDER, CYLINDER) =>
                if allPiecesEmpowered(index2) == true then
                  vectorHealth(index1) += 2
                  vectorHealth(index1) += 1
                  scribe.info("@@@ {Cyldr" + color1 +":" + 1 + "} {Cyldr" + color2 + "}")
              case (BLOCK, BLOCK) =>
                if allPiecesEmpowered(index2) == true then
                  vectorHealth(index1) += 2
                  scribe.info("@@@ {Block" + color1 +":" + 2 + "} {Block" + color2 + "}")
                else
                  vectorHealth(index1) += 1
                  scribe.info("@@@ {Block" + color1 +":" + 1 + "} {Block" + color2 + "}")

              case (CYLINDER, BLOCK) =>
                if allPiecesEmpowered(index2) == true then
                  vectorHealth(index1) -= 2
                  vectorHealth(index2) += 2
                  scribe.info("@@@ <Cyldr" + color1 + ":" + (-2) + "> <Block" + color2 +":" + 2 + ">")
                else
                  vectorHealth(index1) -= 1
                  vectorHealth(index2) += 1
                  scribe.info("@@@ <Cyldr" + color1 + ":" + (-1) + "> <Block" + color2 +":" + 1 + ">")
              case (BLOCK, CYLINDER) =>
                if allPiecesEmpowered(index2) == true then
                  vectorHealth(index1) -= 2
                  vectorHealth(index2) += 2
                  scribe.info("@@@ <Block" + color1 + ":" + (-2) + "> <Cyldr" + color2 +":" + 2 + ">")
                else
                  vectorHealth(index1) -= 1
                  vectorHealth(index2) += 1
                  scribe.info("@@@ <Block" + color1 + ":" + (-1) + "> <Cyldr" + color2 +":" + 1 + ">")
            }
          end if
        end if
      }
    }
    scribeCombat(allPieces, vectorHealth)

    val piecesWithCaptures = captured(allPieces, vectorHealth)
    (piecesWithCaptures)

  end combat

  /*  captured is used during the turn to mark unhealthy pieces as captured
   */

  def captured(allPieces: Vector[Piece], vectorHealth: Array[Int]) : Pieces = 
    var newPieces = Vector.empty[(Piece)]
    allPieces.foreach { piece =>
      val index = (piece.pieceShape * 6) + piece.pieceIdentity
      val newPiece = 
        if vectorHealth(index) < 0 then 
          Piece.setCaptured(piece, true)
        else
          Piece.setCaptured(piece, false)
        end if
      newPieces = newPieces :+ newPiece
    }
    Pieces(newPieces )
  end captured

  /* detectCaptors is used at the end of a turn to revisits all captives to ...
  .. calculate the set of captors. If there has been no captures this turn ...
  .. an empty set is generated indicating it is ok to transition the next move ...
  .. to opponent
   */

  def detectCaptors(model: FlicFlacGameModel) : Set[(Piece)] =
    val allPieces = model.pieces.modelPieces
    val vPrisoners = allPieces.filter(p => p.bCaptured == true)
    var setCaptors = Set.empty[(Piece)]
    
    vPrisoners.foreach { p =>
      val qrs = model.hexBoard3.getQRSfromAxAy(p.pCurPos.x, p.pCurPos.y)
      val spotQRS = model.possibleMoveSpots.spotRingQRS(qrs._1, qrs._2, qrs._3)
      val prisonerColor = p.pieceIdentity
      allPieces.foreach { pp =>
        val qrs1 = model.hexBoard3.getQRSfromAxAy(pp.pCurPos.x, pp.pCurPos.y)
        if spotQRS.contains(qrs1) then
          val captorColor1 = if pp.bFlipped then (pp.pieceIdentity + 4) % 6 else (pp.pieceIdentity + 1) % 6
          val captorColor2 = if pp.bFlipped then (pp.pieceIdentity + 5) % 6 else (pp.pieceIdentity + 2) % 6
          if prisonerColor == captorColor1 || prisonerColor == captorColor2 then
            setCaptors = setCaptors + pp
          end if 
        end if
      }
    }
    (setCaptors)
  end detectCaptors

  /* rewardCaptors is used at the end of the turn to reset the captors so that they ...
  .. can move again. It marks all other pieces as moved (even if they not moved)
  .. If a piece (including a captor) is captured then it returns to home position.
  
   */

  def rewardCaptors(model: FlicFlacGameModel, setCaptors: Set[(Piece)]) : Pieces =
    var allPieces = Vector.empty[(Piece)]
    model.pieces.modelPieces.foreach { p=>
      if setCaptors.contains(p) then
        val p1 = Piece.setCaptor(p, false)
        val p2 = Piece.setMoved(p1, false)
        if Piece.captured(p2) then
          val p3 = Piece.moveToHome(p2)
          val p4 = Piece.setTurnStartPos(p3, p3.pCurPos)
          allPieces = allPieces :+ p4        
        else
          val p3 = Piece.setTurnStartPos(p2, p2.pCurPos)
          allPieces = allPieces :+ p3
        end if
      else
        val p1 = Piece.setMoved(p, true)
        if Piece.captured(p1) then
          val p2 = Piece.moveToHome(p1)
          val p3 = Piece.setTurnStartPos(p2, p2.pCurPos)
          allPieces = allPieces :+ p3
        else
          allPieces = allPieces :+ p
        end if
      end if
    }
    Pieces(allPieces)
    
  end rewardCaptors


  def scribeCombat(allPieces: Vector[Piece], vectorHealth: Array[Int]) : Unit = 
    val (cylinders, blocks) = allPieces splitAt 6 // Cylinders are 0...5 & blocks are 6...11
    var str1 = "Cyldrs "
    cylinders.foreach { p =>
      val color = PieceAssets.pieceNames(p.pieceIdentity)
      val index = (p.pieceShape * 6) + p.pieceIdentity
      str1 = str1 + "<" + color.trim + ":" + vectorHealth(index) + ">"
    }
    scribe.debug("@@@ " + str1)

    var str2 = "Blocks "
    blocks.foreach { p =>
      val color = PieceAssets.pieceNames(p.pieceIdentity)
      val index = (p.pieceShape * 6) + p.pieceIdentity
      str2 = str2 + "<" + color.trim + ":" + vectorHealth(index) + ">"
    }
    scribe.debug("@@@ " + str2)

  end scribeCombat

object Melee {}

end Melee
