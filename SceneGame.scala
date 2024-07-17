package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*

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

// --- Here we have the intrusion of the hex game objects ...
// format: off
  val boardCfg = BoardConfig(
    91,                             // GWIDTH pixel width of graphic
    81,                             // GHEIGHT pixel height of graphic
    Point(260,30),                  // where the (inisible) top left hand corner of the hex grid board is positioned
    3,                              // game size
    70,                             // amount to add to a hex centre x coord to reach the vertical line of the next column
    40,                             // half the amount to add to a hex centre y coord to reach the next hexagon below
    10                              // xcoord of halfway along the top left diagonal line of first hex
  )
// format: on

  var iFrameTick = 0
  var iDragTick = 0
  var dMsg = "-----"

  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  val hexBoard = HexBoard(boardCfg, scaleFactor)
  
  val pieces = Pieces(
    //boardCfg,
    hexBoard
  )

  val highLighter = HighLighter(boardCfg, hexBoard, scaleFactor)

// --- End of hex game intrusion

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {

    case e: PointerEvent.PointerDown =>
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Down, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) => 
              // Pointer Down, Pos on Grid, Piece Selected
              if (piece.pCurPos == pos) then
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos <<##A##>>
                dMsg = "##AA## Down|Grid|Sel|=="
                println("@@@ PointerEvent " + dMsg)
                highLighter.setPos(pos)
                highLighter.shine(true)
                val updatedPiece = Piece.setSelected(piece,true)  // FIXME need to record immutability
                Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))
              else 
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##B##>>
                dMsg = "##B## Down|Grid|Sel|!="
                println("@@@ PointerEvent " + dMsg)
                highLighter.shine(false)
                if (hexBoard.isThisHexBlack(pos) == true) then 
                  val updatedPiece = Piece.setPosFlipDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))
                else
                  val updatedPiece = Piece.setPosDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))
                end if
              end if

            case None =>
              // Pointer Down, Pos on Grid, No Piece Selected
              FlicFlacGameModel.findPieceByPos(model, pos) match
                case Some(piece) =>
                  // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos <<##C##>>
                  dMsg = "##C## Down|Grid|Non|=="
                  println("@@@ PointerEvent " + dMsg)
                  highLighter.setPos(pos)
                  highLighter.shine(true)
                  val updatedPiece = Piece.setSelected(piece,true)
                  println("@@@ updatePiece Select is ... " + updatedPiece.bSelected)
                  val outcome = FlicFlacGameModel.modify(model, Some(updatedPiece))
                  Outcome(outcome)

                case None => 
                  // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found <<##D##>>
                  dMsg = "##D## Down|Grid|Non|!="
                  println("@@@ PointerEvent " + dMsg)
                  highLighter.setPos(pos)
                  highLighter.shine(true)
                  Outcome(FlicFlacGameModel.modify(model, None))

              end match // findPieceByPos
          end match // findPieceSelected

        case None =>
          // Pointer Down, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Down, Pos off Grid, Piece Selected <<##E##>>
              dMsg = "##E## Down|Void|Sel"
              println("@@@ PointerEvent " + dMsg)
              highLighter.shine(false)              
              val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
              Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))

            case None =>
              // Pointer Down, Pos off Grid, No Piece Selected <<##F>>
              dMsg ="##F## Down|Void|Non"
              println("@@@ PointerEvent " + dMsg)
              highLighter.shine(false)
              Outcome(FlicFlacGameModel.modify(model, None))
      end match // hexXYCoordsFromDisplayXY

    case e: PointerEvent.PointerUp =>    
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Up, Pos on Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos on Grid, Piece Selected
              if (piece.pCurPos == pos) then
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos==PointerPos <<##G##>>
                dMsg = "##G## Up|Grid|Sel|=="
                println("@@@ PointerEvent " + dMsg)
                Outcome(FlicFlacGameModel.modify(model, None))
              else 
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos!=PointerPos <<##H##>>
                dMsg = "##H## Up|Grid|Sel|!="
                println("@@@ PointerEvent " + dMsg)
                highLighter.shine(false)
                if (hexBoard.isThisHexBlack(pos) == true) then 
                  val updatedPiece = Piece.setPosFlipDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))
                else
                  val updatedPiece = Piece.setPosDeselect(piece, pos)
                  Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))
                end if

            case None =>
              // Pointer Up, Pos on Grid, No piece selected <<##I##>>
              dMsg = "##I## Up|Grid|Non"
              println("@@@ PointerEvent " + dMsg)
              Outcome(FlicFlacGameModel.modify(model, None))
          end match // findPieceSelected
          
        case None =>
          // Pointer Up, Pos off Grid
          FlicFlacGameModel.findPieceSelected(model) match
            case Some(piece) =>
              // Pointer Up, Pos off Grid, Piece Selected <<##J##>>
              dMsg = "##J## Up|Void|Sel"
              println("@@@ PointerEvent " + dMsg)
              highLighter.shine(false)              
              val updatedPiece = Piece.setPosDeselect(piece, piece.pHomePos)
              Outcome(FlicFlacGameModel.modify(model, Some(updatedPiece)))

            case None => 
              // Pointer Up, Pos off Grid, No piece selected <<##K##>>
              dMsg = "##K## Up|Void|Non"
              println("@@@ PointerEvent " + dMsg)
              highLighter.shine(false)
              Outcome(FlicFlacGameModel.modify(model, None))
          end match // findPieceSelected

          
      end match // hexXYCoordsFromDisplayXY      

    case _ => 
      Outcome(FlicFlacGameModel.modify(model, None))
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
      if (viewModel.dragOn) then 
        FlicFlacGameModel.findPieceSelected(model) match
          case Some(p) =>
            println("@@@ PointerEventMove @ " + e.position)
            viewModel.optDragPos = Some(e.position)

          case None =>
            viewModel.optDragPos = None
      end if
      iDragTick += 1
      Outcome(viewModel)

    case ButtonRoundEvent =>
      if (viewModel.dragOn) then viewModel.dragOn = false
      else viewModel.dragOn = true
      Outcome(viewModel)

    case _ => 
      Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.    val x = context.

  def present(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel,
      viewModel: SceneViewModel
  ): Outcome[SceneUpdateFragment] =

//    val textGame = TextBox("Game Scene")
    val textGame = TextBox(dMsg +" "+iFrameTick+":"+iDragTick, 1000, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)

    val dragState = if (viewModel.dragOn) then "Drag:ON" 
                    else "DRAG:OFF"
    
    val textDrag = TextBox(dragState, 200, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(110, 60)

    val bootData = context.frameContext.startUpData.flicFlacBootData

    val width = bootData.pixelWidth
    val height = bootData.pixelHeight

    Outcome(SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.White)))
                    |+| SceneUpdateFragment(textGame)
                    |+| SceneUpdateFragment(viewModel.dragButton.draw)
                    |+| SceneUpdateFragment(textDrag)
                    |+| SceneUpdateFragment(viewModel.splashButton.draw)
                    |+| SceneUpdateFragment(viewModel.rulesButton.draw)
//                  |+| SceneUpdateFragment(viewModel.gameButton.draw)
                    |+| SceneUpdateFragment(viewModel.resultsButton.draw)
                    |+| hexBoard.paint(scaleFactor)
                    |+| highLighter.paint(scaleFactor)
                    |+| pieces.paint(model, scaleFactor, viewModel.optDragPos)
    )
    

final case class GameSceneViewModel(
  var optDragPos : Option[Point],
  var dragOn : Boolean,
  dragButton: Button,
  splashButton: Button,
  rulesButton: Button,
//  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for {
      bn0 <- dragButton.updateFromPointers(pointers)
      bn1 <- splashButton.updateFromPointers(pointers)
      bn2 <- rulesButton.updateFromPointers(pointers)
//      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    } yield this.copy( dragButton = bn0, splashButton = bn1, rulesButton = bn2, /*gameButton = bn3,*/ resultsButton = bn4)

object GameSceneViewModel:

  val initial: GameSceneViewModel = 

    GameSceneViewModel(

      None,   // we have no last position of the pointer recorded

      false,  // the drag option is not switched on

      Button (
        buttonAssets = GameAssets.buttonRoundAssets,
        bounds = Rectangle(20, 40, 90, 80),
        depth = Depth(6),
      ).withUpActions(ButtonRoundEvent),

      Button (
        buttonAssets = GameAssets.buttonSplashAssets,
        bounds = Rectangle(20, 120, 240, 80),
        depth = Depth(6)
        ).withUpActions(ButtonSplashEvent),

      Button (
        buttonAssets = GameAssets.buttonRulesAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonRulesEvent),
/*-
      Button (
        buttonAssets = GameAssets.buttonGameAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonGameEvent),
*/
      Button (
        buttonAssets = GameAssets.buttonResultsAssets,
        bounds = Rectangle(20, 320, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonResultsEvent)
    )

/* Adjusting buttons requires the following...
1) Add/Remove Button from Object GameSceneViewModel
2) Adjust positions of buttons in Object GameSceneViewModel
3) Add/Remove pointer handling in case class GameSceneViewModel / update
4) Add/Remove appropriate SceneUpdateFragment in present / outcome

 */    
    
