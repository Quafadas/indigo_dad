package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network

object SceneGame extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:

  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  val name: SceneName = SceneName("Game")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty
  
  var bBlinkOn = true
  var dMsg = "-----"


  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {
    e =>
      try {
        e match
          case e: FlicFlacGameUpdate.Info =>
            scribe.debug("@@@ FlicFlacGameUpdate.Info")
            FlicFlacGameModel.modify(e.ffgm, None, None)
            Outcome(e.ffgm)

          case e: PointerEvent.PointerDown =>
            val clickPoint = e.position
            val hexPosn = hexBoard4.getAxAyFromDisplayXY(clickPoint, hexBoard4.scalingFactor)
            hexPosn match
              case Some(pos) =>
                // Pointer Down, Pos on Grid
                checkTurn(model,"PDOWN")
                FlicFlacGameModel.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Down, Pos on Grid, Piece Selected
                    if piece.pCurPos == pos then
                      // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos <<##A##>>
                      dMsg = "##A##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.setPosAndShine(pos)
                      val modelA1 = model.copy(highLighter = newHL)
                      val updatedPiece = Piece.setSelected(piece, true)
                      FlicFlacGameModel.modify(modelA1, Some(updatedPiece), None)
                    else
                      // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##B##>>
                      dMsg = "##B##"
                      scribe.debug("@@@ PointerEvent " + dMsg)
                      val newHL = model.highLighter.shine(false)
                      Outcome(model.copy(highLighter = newHL))
                    end if

                  case None =>
                    // Pointer Down, Pos on Grid, No Piece Selected
                    FlicFlacGameModel.findPieceByPos(model, pos) match
                      case Some(piece) =>
                        if ((piece.pieceShape == CYLINDER) && (model.gameState == GameState.CYLINDER_TURN))
                          || ((piece.pieceShape == BLOCK) && (model.gameState == GameState.BLOCK_TURN))
                        then
                          // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos and correct turn <<##C##>>
                          dMsg = "##C##"
                          scribe.debug("@@@ PointerEvent " + dMsg)
                          val newHL = model.highLighter.setPosAndShine(pos)
                          val updatedPiece = Piece.setSelected(piece, true)
                          FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
                        else
                          // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos but incorrect turn <<##D##>>
                          dMsg = "##D##"
                          scribe.debug("@@@ PointerEvent " + dMsg)
                          Outcome(model)

                      case None =>
                        // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found <<##E##>>
                        dMsg = "##E##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.setPosAndShine(pos)
                        scribe.debug("@@@ FIXME:" + newHL.currentPos + ":" + newHL.displayOn)
                        FlicFlacGameModel.modify(model, None, Some(newHL))

                    end match // findPieceByPos
                end match // findPieceSelected

              case None =>
                // Pointer Down, Pos off Grid
                FlicFlacGameModel.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Down, Pos off Grid, Piece Selected <<##F##>>
                    dMsg = "##F##"
                    scribe.debug("@@@ PointerEvent " + dMsg)
                    val newHL = model.highLighter.shine(false)
                    val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
                    FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))

                  case None =>
                    // Pointer Down, Pos off Grid, No Piece Selected <<##G##>>
                    dMsg = "##G##"
                    scribe.debug("@@@ PointerEvent " + dMsg)
                    val newHL = model.highLighter.shine(false)
                    FlicFlacGameModel.modify(model, None, Some(newHL))
                      .addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, "")) // this clears any panel showing
            end match // hexXYCoordsFromDisplayXY

          case e: PointerEvent.PointerUp =>
            val clickPoint = e.position
            val hexPosn = hexBoard4.getAxAyFromDisplayXY(clickPoint, hexBoard4.scalingFactor)
            hexPosn match
              case Some(pos) =>
                // Pointer Up, Pos on Grid
                FlicFlacGameModel.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Up, Pos on Grid, Piece Selected
                    if model.possibleMoveSpots.indices((pos.x, pos.y)) then
                      // Pointer Up, Pos on Grid, Piece Selected, Valid Move
                      val newHL = model.highLighter.shine(false)
                      if hexBoard4.isThisHexBlack(pos) == true && piece.bMoved == false then
                        // we only flip piece if this is a new move
                        dMsg = "##H##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val updatedPiece = Piece.setPosFlipDeselect(piece, pos)

                        FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)).flatMap { um =>
                          val newPieces = Melee(um).combat(um)
                          FlicFlacGameModel.modifyPieces(um, newPieces)
                        }
                      else
                        dMsg = "##I##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val updatedPiece = Piece.setPosDeselect(piece, pos)
                        FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel)
                          FlicFlacGameModel.modifyPieces(updatedModel, newPieces)
                        }
                      end if
                    else
                      // Pointer Up, Pos on Grid, Piece Selected
                      if pos == piece.pCurPos then
                        // Pointer Up, Pos on Grid, Piece Selected, No Move
                        dMsg = "##J##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.shine(true)
                        val updatedPiece = Piece.setSelected(piece, true)
                        FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel)
                          FlicFlacGameModel.modifyPieces(updatedModel, newPieces)
                        }
                      else
                        // Pointer Up, Pos on Grid, Piece Selected, Invalid Move
                        dMsg = "##K##"
                        scribe.debug("@@@ PointerEvent " + dMsg)
                        val newHL = model.highLighter.shine(false)
                        val updatedPiece = Piece.setPosDeselect(piece, piece.pCurPos)
                        FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                          val newPieces = Melee(updatedModel).combat(updatedModel)
                          FlicFlacGameModel.modifyPieces(updatedModel, newPieces)
                        }
                      end if
                    end if

                  case None =>
                    // Pointer Up, Pos on Grid, No piece selected
                    dMsg = "##L##"
                    scribe.debug("@@@ PointerEvent " + dMsg)

                    // FIXME 8 test lines to show magenta hex detail follows ...

                    val w = pos.x
                    val h = pos.y
                    val x = hexBoard4.hexArray(w)(h).x
                    val y = hexBoard4.hexArray(w)(h).y
                    val q = hexBoard4.hexArray(w)(h).q
                    val r = hexBoard4.hexArray(w)(h).r
                    val s = hexBoard4.hexArray(w)(h).s
                    scribe.debug(
                      "@@@ Magenta hexboard3: (w,h) x,y,q,r,s = (" + w + "," + h + ") : "
                        + x + "," + y + " : " + q + "," + r + "," + s
                    )
                    FlicFlacGameModel.modify(model, None, None)

                end match // findPieceSelected

              case None =>
                // Pointer Up, Pos off Grid
                FlicFlacGameModel.findPieceSelected(model) match
                  case Some(piece) =>
                    // Pointer Up, Pos off Grid, Piece Selected
                    dMsg = "##M##"
                    scribe.debug("@@@ PointerEvent " + dMsg)
                    val newHL = model.highLighter.shine(false)
                    val updatedPiece = Piece.setPosDeselect(piece, piece.pCurPos)
                    FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)).flatMap { updatedModel =>
                      val newPieces = Melee(updatedModel).combat(updatedModel)
                      FlicFlacGameModel.modifyPieces(updatedModel, newPieces)
                    }

                  case None =>
                    // Pointer Up, Pos off Grid, No piece selected
                    dMsg = "##N##"
                    scribe.debug("@@@ PointerEvent " + dMsg)
                    val newHL = model.highLighter.shine(false)
                    FlicFlacGameModel.modify(model, None, Some(newHL))
                end match // findPieceSelected

            end match // hexXYCoordsFromDisplayXY

          case ButtonNewGameEvent =>
            checkTurn(model,"NEWGAME")
            Outcome(FlicFlacGameModel.reset(model))

          case ButtonPlusEvent =>
            scribe.debug("@@@ ButtonPlusEvent")
            val oldSF = hexBoard4.scalingFactor
            val newSF = increaseScaleFactor(oldSF)
            hexBoard4.calculateXsYs(newSF)
            hexBoard4.calculateGridPaintLayer()
            Outcome(model)

          case ButtonMinusEvent =>
            scribe.debug("@@@ ButtonMinusEvent")
            val oldSF = hexBoard4.scalingFactor
            val newSF = decreaseScaleFactor(oldSF)
            hexBoard4.calculateXsYs(newSF)
            hexBoard4.calculateGridPaintLayer()
            Outcome(model)

          case ViewportResize(gameViewPort) =>
            var dSF = 1.0
            if (FlicFlacGameModel.getStartUpStates().contains(model.gameState)) then
              scribe.debug("@@@ ViewPortResize from scratch")
              val w = gameViewPort.width - hexBoard4.pBase.x
              val h = gameViewPort.height - hexBoard4.pBase.y
              dSF = GetScaleFactor(w, h, GameAssets.GameSceneDimensions)
              scribe.debug("@@@ updateModel ViewportResize w:h->s " + w + ":" + h + "->" + dSF)
            else
              dSF = hexBoard4.scalingFactor
              scribe.debug("@@@ ViewPortResize from previous model sf=" + dSF)
            end if

            hexBoard4.calculateXsYs(dSF)
            hexBoard4.calculateGridPaintLayer()

            val newModel = model.copy(gameState = GameState.CYLINDER_TURN)
            FlicFlacGameModel.modify(newModel, None, None)

          case ButtonTurnEvent.Occurence() =>
            scribe.debug("@@@ ButtonTurnEvent")
            checkTurn(model, "BTURN")
            val emptySpots = Spots(Set.empty)
            val newScore = model.pieces.extraTurnScoring(model)
            val captors = Melee(model).detectCaptors(model)
            if captors.isEmpty then
              val newTT = TurnTimer.restartForTurn(model.turnTimer)
              val newPieces = model.pieces.newTurn(model)
              val newGameState = 
                if model.gameState == GameState.CYLINDER_TURN then
                  scribe.debug("@@@@ BLOCK TURN @@@")
                  GameState.BLOCK_TURN
                else
                  scribe.debug("@@@ CYLINDER TURN @@@")
                  GameState.CYLINDER_TURN
                end if

              val newModel = model.copy(
                gameState = newGameState,
                pieces = newPieces,
                possibleMoveSpots = emptySpots,
                gameScore = newScore,
                turnTimer = newTT
              )
              scribe.debug("@@@ " + model.gameState.toString() + " -> " + newModel.gameState.toString()) 
              FlicFlacGameModel.modify(newModel, None, None)
            else
              scribe.debug("@@@ CAPTORS @@@")
              val newTT = TurnTimer.restartForCaptors(model.turnTimer)
              val newPieces = Melee(model).rewardCaptors(model, captors)
              val newModel = model.copy(pieces = newPieces, possibleMoveSpots = emptySpots, gameScore = newScore, turnTimer = newTT)
              FlicFlacGameModel.modify(newModel, None, None)
            end if

          // Keyboard Interface for testing purposes only ...
          case k: KeyboardEvent.KeyDown =>
            if k.keyCode == Key.ADD then Outcome(model).addGlobalEvents(ButtonPlusEvent)
            else if k.keyCode == Key.SUBTRACT then Outcome(model).addGlobalEvents(ButtonMinusEvent)
            else if k.keyCode == Key.ENTER then Outcome(model).addGlobalEvents(ButtonTurnEvent.Occurence())
            else if k.keyCode == Key.F3 then Outcome(model).addGlobalEvents(SceneEvent.Previous)
            else if k.keyCode == Key.F4 then Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR,"Test Error from GAME FKEY_F4"))
            else Outcome(model)
            end if

          case FrameTick =>
            val t1 = System.currentTimeMillis / 100 // this is 10ths of a second
            val bNewBlinkOn = if (t1 % 10) > 0 then true else false
            if bNewBlinkOn != bBlinkOn then
              // update the global bBlinkOn
              bBlinkOn = bNewBlinkOn
            end if

            if TurnTimer.expired(model.turnTimer) then
              val bCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER)
              val bBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK)
              if (bCylinder == true) || (bBlock == true) then
                // signal a button turn event to switch players
                Outcome(model).addGlobalEvents(ButtonTurnEvent.Occurence())
              else
                Outcome(model)
              end if
            else
              val possibleTT = TurnTimer.update(model.turnTimer)
              possibleTT match
                case Some(tt) =>
                  Outcome(model.copy(turnTimer = tt))
                case None =>
                  Outcome(model)
              end match
            end if

          case _ =>
            Outcome(model)
      } 
      catch {
          case t: Throwable =>
            scribe.error("SceneGame updateModel " + t.getMessage())
            Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, t.getMessage()))
      }
  
  } // end of GlobalEvent => Outcome[FlicFlacGameModel]

  end updateModel

  def checkTurn(model: FlicFlacGameModel, errPos: String) : Unit = 
    val bCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == BLOCK)
    val bBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == CYLINDER)
    if (bCylinder == true) || (bBlock == true) then
      throw new Exception(errPos + " ... Please wait for your turn")
    end if
  end checkTurn
  
  def increaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF >= 0.9 then 1.0
      else if oldSF >= 0.8 then 0.9
      else if oldSF >= 0.75 then 0.8
      else if oldSF >= 0.67 then 0.75
      else if oldSF >= 0.5 then 0.67
      else 0.5
    scribe.debug("@@@ increaseScaleFactor to:" + newSF)
    newSF
  end increaseScaleFactor

  def decreaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF <= 0.5 then 0.33
      else if oldSF <= 0.67 then 0.5
      else if oldSF <= 0.75 then 0.67
      else if oldSF <= 0.8 then 0.75
      else if oldSF <= 0.9 then 0.8
      else 0.9
    scribe.debug("@@@ decreaseScaleFactor to:" + newSF)
    newSF
  end decreaseScaleFactor

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case e: PointerEvent.PointerMove =>
      FlicFlacGameModel.findPieceSelected(model) match
        case Some(p) =>
          scribe.trace("@@@ PointerEventMove @ " + e.position)
          viewModel.optDragPos = Some(e.position)

        case None =>
          viewModel.optDragPos = None
      end match
      Outcome(viewModel)

    case ButtonPlusEvent =>
      val newViewModel = viewModel.changeButtonBoundaries(model, viewModel.gameViewport)
      Outcome(newViewModel)

    case ButtonMinusEvent =>
      val newViewModel = viewModel.changeButtonBoundaries(model, viewModel.gameViewport)
      Outcome(newViewModel)

    case ViewportResize(gameViewPort) =>
      val newViewModel = viewModel.changeButtonBoundaries(model, gameViewPort)
      Outcome(newViewModel)

    case _ =>
      Outcome(viewModel)
  end updateViewModel

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val dSF = hexBoard4.scalingFactor
    val sFactor = ((10 * dSF).toInt).toString()

    val textDiag = TextBox(sFactor + " " + dMsg, 200, 40) // FIXME 200,40 just some convenient numbers for text box size
      .withColor(RGBA.Black)
      .withFontSize(Pixels(20))
      .moveTo(0, 0)

    val cylinderName =
      if model.ourPieceType == CYLINDER then model.ourName
      else model.oppoName
      end if
    end cylinderName
    val blockName =
      if model.ourPieceType == CYLINDER then model.oppoName
      else model.ourName
      end if
    end blockName

    val scorePanel = 
      if (bBlinkOn == true ) && (model.gameState == GameState.CYLINDER_TURN) then
        GameAssets.gScorePanelBlinkCylinder(1.0).moveTo(0, 130)
      else if (bBlinkOn == true ) && (model.gameState == GameState.BLOCK_TURN) then
        GameAssets.gScorePanelBlinkBlock(1.0).moveTo(0, 130)
      else
        GameAssets.gScorePanelBlinkOff(1.0).moveTo(0, 130)
      end if
 
    val cylinderPlayer =
      TextBox((cylinderName).toString(), 220, 50)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .moveTo(14, 190)

    val blockPlayer =
      TextBox((blockName).toString(), 220, 50)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(40))
        .moveTo(14, 370)

    val cylinderScoreX = coordXFromScore(model.gameScore._1)
    val blockScoreX = coordXFromScore(model.gameScore._2)

    val cylinderScore =
      TextBox((model.gameScore._1).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .moveTo(cylinderScoreX, 250)
    val blockScore =
      TextBox((model.gameScore._2).toString(), 150, 300)
        .withColor(RGBA.Black)
        .withFontSize(Pixels(100))
        .moveTo(blockScoreX, 430)
    val param1 = 
      TextBox((model.winningScore).toString(),100,70)
        .bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .moveTo(140, 670)
    val param2 = 
      TextBox((model.turnTimer.iTotalTurnTime).toString(),100,70)
        .bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .moveTo(140, 670+85)
    val param3 = 
      TextBox((model.turnTimer.iCaptorsTurnTime).toString(),100,70)
        .bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .moveTo(140, 670+170)
    val param4 = 
      TextBox((model.randEventFreq).toString(),100,70)
        .bold
        .withColor(RGBA.Black)
        .withFontSize(Pixels(60))
        .moveTo(140, 670+255)

    val pB = hexBoard4.pBase // ................... for HighLighter

    val width = GameAssets.GameSceneDimensions.width
    val height = GameAssets.GameSceneDimensions.height

    val iHeight = (math.round(GameAssets.GameSceneDimensions.height * dSF)).toInt
    val iLeftWidth = hexBoard4.pBase.x
    val iRightWidth = (math.round(GameAssets.GameSceneDimensions.right - hexBoard4.pBase.x) * dSF).toInt
    val rLeft = Rectangle(0, 0, iLeftWidth, iHeight)
    val rRight = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth, iHeight))
    val rCorners = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth + hexBoard4.pBase.x, iHeight))

    val colorOurNameTitle = 
      if (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER) then 
        RGBA.Magenta
      else if (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK) then 
        RGBA.Magenta
      else
        RGBA.Black

    val youAre = 
      TextBox(model.ourName +"    ", iRightWidth, 50)  // adding 4 spaces to get a simple central alignment
        .bold
        .alignCenter
        .withColor(colorOurNameTitle)
        .withFontSize(Pixels(40))
        .moveTo(iLeftWidth, 2)

    Outcome(
        SceneUpdateFragment(LayerKeys.Background -> Layer.empty)        
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black))))
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rLeft, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(rRight, Fill.Color(RGBA.White))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(GameAssets.cornerLayers(rCorners, 1.0, RGBA.Magenta)))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Magenta))))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(textDiag))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.turnButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(youAre))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(scorePanel))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(cylinderPlayer))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(blockPlayer))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(cylinderScore))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(blockScore))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(GameAssets.gParamsPanel(1.0).moveTo(0, 590)))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(Batch(param1, param2, param3, param4)))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.plusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.minusButton.draw))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(viewModel.newGameButton.draw))      
        |+| SceneUpdateFragment(LayerKeys.Middleground -> TurnTimer.show(model))
        |+| SceneUpdateFragment(LayerKeys.Middleground -> hexBoard4.paint(model, dSF))
        |+| SceneUpdateFragment(LayerKeys.ForegroundHighL -> model.highLighter.paint(hexBoard4, dSF, pB))
        |+| SceneUpdateFragment(LayerKeys.ForegroundSpots -> model.possibleMoveSpots.paint(model))
        |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> model.pieces.paint(model, dSF, bBlinkOn, viewModel.optDragPos))

    )
  end present

  def coordXFromScore(score: Int): Int =
    if score < 10 then 150
    else 120
  end coordXFromScore

end SceneGame

final case class GameSceneViewModel(
    var optDragPos: Option[Point],
    gameViewport: GameViewport,
    newGameButton: Button,
    plusButton: Button,
    minusButton: Button,
    turnButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for
      bn1 <- newGameButton.updateFromPointers(pointers)
      bn2 <- plusButton.updateFromPointers(pointers)
      bn3 <- minusButton.updateFromPointers(pointers)
      bn4 <- turnButton.updateFromPointers(pointers)
    yield this.copy(newGameButton = bn1, plusButton = bn2, minusButton = bn3, turnButton = bn4)

  def changeButtonBoundaries(model: FlicFlacGameModel, gvp: GameViewport): GameSceneViewModel =
    // val dSF = model.scalingFactor
    // scribe.debug("@@@ dSF:" + dSF)
    // Current implementation does not require buttons to scale so we override the scaling factor to 1.0
    val dSF = 1.0

    val newNewGameButton =
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.newGameBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent)

    val newPlusButton =
      Button(
        buttonAssets = GameAssets.buttonPlusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.plusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonPlusEvent)

    val newMinusButton =
      Button(
        buttonAssets = GameAssets.buttonMinusAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.minusBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonMinusEvent)

    val newTurnButton =
      Button(
        buttonAssets = GameAssets.buttonTurnAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.turnBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonTurnEvent.Occurence())

    this.copy(
      // scalingFactor
      // optDragPos
      newGameButton = newNewGameButton,
      plusButton = newPlusButton,
      minusButton = newMinusButton,
      turnButton = newTurnButton
    )

  end changeButtonBoundaries

end GameSceneViewModel

object GameSceneViewModel:
  val turnBounds = Rectangle(10, 30, 90, 80)
  val plusBounds = Rectangle(10, 1020, 90, 80)
  val minusBounds = Rectangle(160, 1020, 90, 80)
  val newGameBounds = Rectangle(10, 1120, 240, 80)

  val initial: GameSceneViewModel =
    GameSceneViewModel(
      None, // ... we have no last position of the pointer recorded

      GameViewport(GameAssets.GameSceneDimensions.width, GameAssets.GameSceneDimensions.height),
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(1.0),
        bounds = newGameBounds,
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent),
      Button(
        buttonAssets = GameAssets.buttonPlusAssets(1.0),
        bounds = plusBounds,
        depth = Depth(6)
      ).withUpActions(ButtonPlusEvent),
      Button(
        buttonAssets = GameAssets.buttonMinusAssets(1.0),
        bounds = minusBounds,
        depth = Depth(6)
      ).withUpActions(ButtonMinusEvent),
      Button(
        buttonAssets = GameAssets.buttonTurnAssets(1.0),
        bounds = turnBounds,
        depth = Depth(6)
      ).withUpActions(ButtonTurnEvent.Occurence())
    )
end GameSceneViewModel

object FlicFlacGameUpdate:
  case class Info(ffgm: FlicFlacGameModel) extends GlobalEvent
end FlicFlacGameUpdate
