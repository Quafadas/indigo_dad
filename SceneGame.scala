package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode

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

  def subSystems = Set(SSGame("SubSystemGame"))

  var bBlinkOn = true
  var dMsg = "-----"

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {

    case e: PointerEvent.PointerDown =>
      val clickPoint = e.position
      val hexPosn = model.hexBoard3.getAxAyFromDisplayXY(clickPoint, model.scalingFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Down, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Down, Pos on Grid, Piece Selected
              if piece.pCurPos == pos then
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos <<##A##>>
                dMsg = "##A##"
                scribe.debug("@@@ PointerEvent " + dMsg)
                val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                val modelA1 = model.copy(highLighter = newHL)
                val updatedPiece = Piece.setSelected(piece, true)
                Outcome(FlicFlacGameModel.modify(modelA1, Some(updatedPiece), None))
              else
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##B##>>
                dMsg = "##B##"
                scribe.debug("@@@ PointerEvent " + dMsg)
                val newHL = model.highLighter.shine(model.highLighter, false)
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
                    val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                    val updatedPiece = Piece.setSelected(piece, true)
                    Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
                  else
                    // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos but incorrect turn <<##D##>>
                    dMsg = "##D##"
                    scribe.debug("@@@ PointerEvent " + dMsg)
                    Outcome(model)

                case None =>
                  // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found <<##E##>>
                  dMsg = "##E##"
                  scribe.debug("@@@ PointerEvent " + dMsg)
                  val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                  Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))

              end match // findPieceByPos
          end match // findPieceSelected

        case None =>
          // Pointer Down, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Down, Pos off Grid, Piece Selected <<##F##>>
              dMsg = "##F##"
              scribe.debug("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
              Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))

            case None =>
              // Pointer Down, Pos off Grid, No Piece Selected <<##G##>>
              dMsg = "##G##"
              scribe.debug("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))
      end match // hexXYCoordsFromDisplayXY

    case e: PointerEvent.PointerUp =>
      val clickPoint = e.position
      val hexPosn = model.hexBoard3.getAxAyFromDisplayXY(clickPoint, model.scalingFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Up, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos on Grid, Piece Selected
              if model.possibleMoveSpots.indices((pos.x, pos.y)) then
                // Pointer Up, Pos on Grid, Piece Selected, Valid Move
                val newHL = model.highLighter.shine(model.highLighter, false)
                if model.hexBoard3.isThisHexBlack(pos) == true && piece.bMoved == false then
                  // we only flip piece if this is a new move
                  dMsg = "##H##"
                  scribe.debug("@@@ PointerEvent " + dMsg)
                  val updatedPiece = Piece.setPosFlipDeselect(piece, pos)
                  val updatedModel = FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
                  val newPieces = Melee(updatedModel).combat(updatedModel)
                  Outcome(FlicFlacGameModel.modifyPieces(updatedModel, newPieces))
                else
                  dMsg = "##I##"
                  scribe.debug("@@@ PointerEvent " + dMsg)
                  val updatedPiece = Piece.setPosDeselect(piece, pos)
                  val updatedModel = FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
                  val newPieces = Melee(updatedModel).combat(updatedModel)
                  Outcome(FlicFlacGameModel.modifyPieces(updatedModel, newPieces))
                end if
              else
                // Pointer Up, Pos on Grid, Piece Selected
                if pos == piece.pCurPos then
                  // Pointer Up, Pos on Grid, Piece Selected, No Move
                  dMsg = "##J##"
                  scribe.debug("@@@ PointerEvent " + dMsg)
                  val newHL = model.highLighter.shine(model.highLighter, true)
                  val updatedPiece = Piece.setSelected(piece, true)
                  val updatedModel = FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
                  val newPieces = Melee(updatedModel).combat(updatedModel)
                  Outcome(FlicFlacGameModel.modifyPieces(updatedModel, newPieces))
                else
                  // Pointer Up, Pos on Grid, Piece Selected, Invalid Move
                  dMsg = "##K##"
                  scribe.debug("@@@ PointerEvent " + dMsg)
                  val newHL = model.highLighter.shine(model.highLighter, false)
                  val updatedPiece = Piece.setPosDeselect(piece, piece.pCurPos)
                  val updatedModel = FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
                  val newPieces = Melee(updatedModel).combat(updatedModel)
                  Outcome(FlicFlacGameModel.modifyPieces(updatedModel, newPieces))
                end if
              end if

            case None =>
              // Pointer Up, Pos on Grid, No piece selected
              dMsg = "##L##"
              scribe.debug("@@@ PointerEvent " + dMsg)

              // FIXME 8 test lines to show magenta hex detail follows ...

              val w = pos.x
              val h = pos.y
              val x = model.hexBoard3.hexArray(w)(h).x
              val y = model.hexBoard3.hexArray(w)(h).y
              val q = model.hexBoard3.hexArray(w)(h).q
              val r = model.hexBoard3.hexArray(w)(h).r
              val s = model.hexBoard3.hexArray(w)(h).s
              scribe.debug(
                "@@@ Magenta hexboard3: (w,h) x,y,q,r,s = (" + w + "," + h + ") : "
                  + x + "," + y + " : " + q + "," + r + "," + s
              )
              Outcome(FlicFlacGameModel.modify(model, None, None))

          end match // findPieceSelected

        case None =>
          // Pointer Up, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos off Grid, Piece Selected
              dMsg = "##M##"
              scribe.debug("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              val updatedPiece = Piece.setPosDeselect(piece, piece.pCurPos)
              val updatedModel = FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL))
              val newPieces = Melee(updatedModel).combat(updatedModel)
              Outcome(FlicFlacGameModel.modifyPieces(updatedModel, newPieces))

            case None =>
              // Pointer Up, Pos off Grid, No piece selected
              dMsg = "##N##"
              scribe.debug("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))
          end match // findPieceSelected

      end match // hexXYCoordsFromDisplayXY

    case ButtonNewGameEvent =>
      Outcome(FlicFlacGameModel.reset(model))

    case ButtonPlusEvent =>
      scribe.debug("@@@ ButtonPlusEvent")
      val oldSF = model.scalingFactor
      val newSF = increaseScaleFactor(oldSF)
      val newHexBoard3 = model.hexBoard3.calculateXpYp(newSF, model.hexBoard3)
      val newModel = model.copy(scalingFactor = newSF, hexBoard3 = newHexBoard3)
      val asJson = newModel.asJson.noSpaces
      org.scalajs.dom.window.localStorage.setItem("FlicFlacStats", asJson)
      Outcome(newModel)

    case ButtonMinusEvent =>
      scribe.debug("@@@ ButtonMinusEvent")
      val oldSF = model.scalingFactor
      val newSF = decreaseScaleFactor(oldSF)
      val newHexBoard3 = model.hexBoard3.calculateXpYp(newSF, model.hexBoard3)
      val newModel = model.copy(scalingFactor = newSF, hexBoard3 = newHexBoard3)
      val asJson = newModel.asJson.noSpaces
      org.scalajs.dom.window.localStorage.setItem("FlicFlacStats", asJson)
      Outcome(newModel)

    case ViewportResize(gameViewPort) =>
      var dSF = 1.0
      if model.gameState == GameState.START then
        scribe.debug("@@@ ViewPortResize from scratch")
        val w = gameViewPort.width - model.hexBoard3.pBase.x
        val h = gameViewPort.height - model.hexBoard3.pBase.y
        dSF = GetScaleFactor(w, h, GameAssets.GameSceneDimensions)
        scribe.debug("@@@ updateModel ViewportResize w:h->s " + w + ":" + h + "->" + dSF)
      else
        dSF = model.scalingFactor
        scribe.debug("@@@ ViewPortResize from previous model sf=" + dSF)
      end if

      val newHexBoard3 = model.hexBoard3.calculateXpYp(dSF, model.hexBoard3)
      // FIXME ... should the cylinders always have the fiest move?
      val newModel = model.copy(scalingFactor = dSF, hexBoard3 = newHexBoard3, gameState = GameState.CYLINDER_TURN)
      val asJson = newModel.asJson.noSpaces
      org.scalajs.dom.window.localStorage.setItem("FlicFlacStats", asJson)
      Outcome(newModel)

    case ButtonTurnEvent =>
      scribe.debug("@@@ ButtonTurnEvent")

      val newScore = model.pieces.extraTurnScoring(model)
      val captors = Melee(model).detectCaptors(model)
      if captors.isEmpty then
        val newPieces = model.pieces.newTurn(model)
        if model.gameState == GameState.CYLINDER_TURN then
          scribe.debug("@@@ BLOCK TURN @@@")
          Outcome(model.copy(gameState = GameState.BLOCK_TURN, pieces = newPieces, gameScore = newScore))
        else
          scribe.debug("@@@ CYLINDER TURN @@@")
          Outcome(model.copy(gameState = GameState.CYLINDER_TURN, pieces = newPieces, gameScore = newScore))
        end if
      else
        val newPieces = Melee(model).rewardCaptors(model, captors)
        Outcome(model.copy(pieces = newPieces, gameScore = newScore))
      end if

    case FrameTick =>
      // FIXME perhaps the blink rate should be configurable
      val t = System.currentTimeMillis / 100 // this is 10ths of a second
      val bNewBlinkOn = if (t % 10) > 0 then true else false
      if bNewBlinkOn != bBlinkOn then
        // update the global bBlinkOn
        bBlinkOn = bNewBlinkOn
      end if
      Outcome(model)
    case _ =>
      Outcome(model)
  }

  end updateModel

  def increaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF >= 0.9 then 1.0
      else if oldSF >= 0.8 then 0.9
      else if oldSF >= 0.75 then 0.8
      else if oldSF >= 0.67 then 0.75
      else if oldSF >= 0.5 then 0.67
      else if oldSF >= 0.33 then 0.5
      else 0.33
    scribe.debug("@@@ increaseScaleFactor to:" + newSF)
    newSF
  end increaseScaleFactor

  def decreaseScaleFactor(oldSF: Double): Double =
    val newSF =
      if oldSF <= 0.33 then 0.25
      else if oldSF <= 0.5 then 0.33
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

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

    val dSF = model.scalingFactor
    val sFactor = ((10 * dSF).toInt).toString()

    val fontSize = (math.round((30 * dSF))).toInt
    val textDiag = TextBox(dMsg, 200, 40) // FIXME 200,40 just some convenient numbers for text box size
      .withColor(RGBA.Black)
      .withFontSize(Pixels(fontSize))
      .moveTo(0, 20)
    val textScore =
      TextBox("Score:" + model.gameScore, 200, 40) // FIXME 200,40 just some convenient numbers for text box size
        .withColor(RGBA.Black)
        .withFontSize(Pixels(fontSize))
        .moveTo(0, 40)

    val bootData = context.frameContext.startUpData.flicFlacBootData // FIXME width and height from wrong source
    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    val iHeight = (math.round(GameAssets.GameSceneDimensions.height * dSF)).toInt
    val iLeftWidth = model.hexBoard3.pBase.x
    val iRightWidth = (math.round(GameAssets.GameSceneDimensions.right - model.hexBoard3.pBase.x) * dSF).toInt
    val rLeft = Rectangle(0, 0, iLeftWidth, iHeight)
    val rRight = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth, iHeight))
    val rCorners = Rectangle(Point(iLeftWidth, 0), Size(iRightWidth + model.hexBoard3.pBase.x, iHeight))

    Outcome(
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black)))
        |+| SceneUpdateFragment(Shape.Box(rLeft, Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(Shape.Box(rRight, Fill.Color(RGBA.White)))
        |+| SceneUpdateFragment(GameAssets.cornerLayers(rCorners, 1.0, RGBA.Magenta))
        |+| SceneUpdateFragment(Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Magenta)))
        |+| SceneUpdateFragment(TextBox(sFactor, 50, 20).withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(0, 0))
        |+| SceneUpdateFragment(textDiag)
        |+| SceneUpdateFragment(textScore)
        |+| SceneUpdateFragment(viewModel.newGameButton.draw)
        |+| SceneUpdateFragment(viewModel.plusButton.draw)
        |+| SceneUpdateFragment(viewModel.minusButton.draw)
        |+| SceneUpdateFragment(viewModel.turnButton.draw)
        |+| model.hexBoard3.paint(model, dSF)
        |+| model.possibleMoveSpots.paint(model)
        |+| model.highLighter.paint(model, dSF)
        |+| model.pieces.paint(model, dSF, bBlinkOn, viewModel.optDragPos)
    )
  end present
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
    val dSF = model.scalingFactor
    scribe.debug("@@@ dSF:" + dSF)

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
      ).withUpActions(ButtonTurnEvent)

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
  val turnBounds = Rectangle(20, 140, 90, 80)
  val newGameBounds = Rectangle(20, 340, 240, 80)
  val plusBounds = Rectangle(20, 440, 90, 80)
  val minusBounds = Rectangle(170, 440, 90, 80)

  val initial: GameSceneViewModel =
    GameSceneViewModel(
      None, // ... we have no last position of the pointer recorded

      GameViewport(1580, 1900),
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
      ).withUpActions(ButtonTurnEvent)
    )
end GameSceneViewModel
