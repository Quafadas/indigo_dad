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

  def subSystems = Set(SSGame("Game"))

  var iFrameTick = 0
  var iDragTick = 0
  var dMsg = "-----"

  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {

    case e: PointerEvent.PointerDown =>
      val clickPoint = e.position
      val hexPosn = model.hexBoard3.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Down, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Down, Pos on Grid, Piece Selected
              if piece.pCurPos == pos then
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos <<##A##>>
                dMsg = "##AA## Down|Grid|Sel|=="
                println("@@@ PointerEvent " + dMsg)
                val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                val updatedPiece = Piece.setSelected(piece, true)
                Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
              else
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##B##>>
                dMsg = "##B## Down|Grid|Sel|!="
                println("@@@ PointerEvent " + dMsg)
                val newHL = model.highLighter.shine(model.highLighter, false)
                if model.hexBoard3.isThisHexBlack(pos) == true then
                  val updatedPiece = Piece.setPosFlipDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
                else
                  val updatedPiece = Piece.setPosDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
                end if
              end if

            case None =>
              // Pointer Down, Pos on Grid, No Piece Selected
              FlicFlacGameModel.findPieceByPos(model, pos) match
                case Some(piece) =>
                  // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos <<##C##>>
                  dMsg = "##C## Down|Grid|Non|=="
                  println("@@@ PointerEvent " + dMsg)
                  val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                  val updatedPiece = Piece.setSelected(piece, true)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))

                case None =>
                  // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found <<##D##>>
                  dMsg = "##D## Down|Grid|Non|!="
                  println("@@@ PointerEvent " + dMsg)
                  val newHL = model.highLighter.setPosAndShine(model.highLighter, pos)
                  Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))

              end match // findPieceByPos
          end match // findPieceSelected

        case None =>
          // Pointer Down, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Down, Pos off Grid, Piece Selected <<##E##>>
              dMsg = "##E## Down|Void|Sel"
              println("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
              Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))

            case None =>
              // Pointer Down, Pos off Grid, No Piece Selected <<##F>>
              dMsg = "##F## Down|Void|Non"
              println("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))
      end match // hexXYCoordsFromDisplayXY

    case e: PointerEvent.PointerUp =>
      val clickPoint = e.position
      val hexPosn = model.hexBoard3.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Up, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos on Grid, Piece Selected
              if piece.pCurPos == pos then
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos==PointerPos <<##G##>>
                dMsg = "##G## Up|Grid|Sel|=="
                println("@@@ PointerEvent " + dMsg)
                Outcome(FlicFlacGameModel.modify(model, None, None))
              else
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##H##>>
                dMsg = "##H## Up|Grid|Sel|!="
                println("@@@ PointerEvent " + dMsg)

                val newHL = model.highLighter.shine(model.highLighter, false)
                if model.hexBoard3.isThisHexBlack(pos) == true then
                  val updatedPiece = Piece.setPosFlipDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
                else
                  val updatedPiece = Piece.setPosDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))
                end if

            case None =>
              // Pointer Up, Pos on Grid, No piece selected <<##I##>>
              dMsg = "##I## Up|Grid|Non"
              println("@@@ PointerEvent " + dMsg)
              Outcome(FlicFlacGameModel.modify(model, None, None))
          end match // findPieceSelected

        case None =>
          // Pointer Up, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos off Grid, Piece Selected <<##J##>>
              dMsg = "##J## Up|Void|Sel"
              println("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
              Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece), Some(newHL)))

            case None =>
              // Pointer Up, Pos off Grid, No piece selected <<##K##>>
              dMsg = "##K## Up|Void|Non"
              println("@@@ PointerEvent " + dMsg)
              val newHL = model.highLighter.shine(model.highLighter, false)
              Outcome(FlicFlacGameModel.modify(model, None, Some(newHL)))
          end match // findPieceSelected

      end match // hexXYCoordsFromDisplayXY

    case ButtonNewGameEvent =>
      Outcome(FlicFlacGameModel.reset(model))

    case _ =>
      Outcome(FlicFlacGameModel.modify(model, None, None))
  }
  end updateModel

  def updateViewModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): GlobalEvent => Outcome[SceneViewModel] =
    case FrameTick =>
      iFrameTick += 1
      viewModel.update(context.mouse, context.frameContext.inputState.pointers)

    case e: PointerEvent.PointerMove =>
      FlicFlacGameModel.findPieceSelected(model) match
        case Some(p) =>
          println("@@@ PointerEventMove @ " + e.position)
          viewModel.optDragPos = Some(e.position)

        case None =>
          viewModel.optDragPos = None
      end match

      iDragTick += 1
      Outcome(viewModel)

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

//    val textGame = TextBox("Game Scene")
    val textGame = TextBox(dMsg + " " + iFrameTick + ":" + iDragTick, 1000, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    val sFactor = ((10*scaleFactor).toInt).toString()

    Outcome(
      SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Cyan)))
        |+| SceneUpdateFragment(Shape.Box(GameAssets.GameSceneDimensions, Fill.Color(RGBA.White)).scaleBy(scaleFactor,scaleFactor))      
        |+| SceneUpdateFragment(GameAssets.cornerLayers(GameAssets.GameSceneDimensions, scaleFactor, RGBA.Magenta))
        |+| SceneUpdateFragment(Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Magenta)))
        |+| SceneUpdateFragment(TextBox(sFactor,50,20).withColor(RGBA.Black).withFontSize(Pixels(20)).moveTo(0,0))
        |+| SceneUpdateFragment(textGame)
        |+| SceneUpdateFragment(viewModel.rulesButton.draw)
        |+| SceneUpdateFragment(viewModel.newGameButton.draw)
        |+| SceneUpdateFragment(viewModel.resultsButton.draw)
        |+| SceneUpdateFragment(viewModel.splashButton.draw)
        |+| model.hexBoard3.paint(model, scaleFactor)
        |+| model.highLighter.paint(model, scaleFactor)
        |+| model.pieces.paint(model, scaleFactor, viewModel.optDragPos)
    )
  end present
end SceneGame

final case class GameSceneViewModel(
    var optDragPos: Option[Point],
    rulesButton: Button,
    newGameButton: Button,
    resultsButton: Button,
    splashButton: Button
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for
      bn1 <- rulesButton.updateFromPointers(pointers)
      bn2 <- newGameButton.updateFromPointers(pointers)
      bn3 <- resultsButton.updateFromPointers(pointers)
      bn4 <- splashButton.updateFromPointers(pointers)
    yield this.copy(rulesButton = bn1, newGameButton = bn2, resultsButton = bn3, splashButton = bn4)

  def changeButtonBoundaries( ssvm : GameSceneViewModel, gvp : GameViewport ) : GameSceneViewModel =
    val dSF = 1.0
    println("@@@ dSF:"+dSF)

    val newRulesButton =       
      Button(
        buttonAssets = GameAssets.buttonRulesAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.rulesBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent)

    val newNewGameButton =       
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.newGameBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent)

    val newResultsButton =       
      Button(
        buttonAssets = GameAssets.buttonResultsAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.resultsBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)

    val newSplashButton =       
      Button(
        buttonAssets = GameAssets.buttonSplashAssets(dSF),
        bounds = GameAssets.scaleButtonBounds(GameSceneViewModel.splashBounds, dSF),
        depth = Depth(6)
      ).withUpActions(ButtonSplashEvent)

    this.copy(  // optDragPos
                rulesButton = newRulesButton,
                newGameButton = newNewGameButton,
                resultsButton = newResultsButton,
                splashButton = newSplashButton
                )
    
  end changeButtonBoundaries

end GameSceneViewModel

object GameSceneViewModel:

  val rulesBounds = Rectangle(20, 40, 240, 80)
  val newGameBounds = Rectangle(20, 140, 240, 80)
  val resultsBounds = Rectangle(20, 240, 240, 80)
  val splashBounds = Rectangle(20, 340, 240, 80)
    
  val initial: GameSceneViewModel =
    GameSceneViewModel(
      None, // we have no last position of the pointer recorded

      Button(
        buttonAssets = GameAssets.buttonRulesAssets(1.0),
        bounds = rulesBounds,
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),
      Button(
        buttonAssets = GameAssets.buttonNewGameAssets(1.0),
        bounds = newGameBounds,
        depth = Depth(6)
      ).withUpActions(ButtonNewGameEvent),
      Button(
        buttonAssets = GameAssets.buttonResultsAssets(1.0),
        bounds = resultsBounds,
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent),
      Button(
        buttonAssets = GameAssets.buttonSplashAssets(1.0),
        bounds = splashBounds,
        depth = Depth(6)
      ).withUpActions(ButtonSplashEvent)
    )
end GameSceneViewModel
