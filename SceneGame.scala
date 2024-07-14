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
  var dMsg1 = "-"
  var dMsg2 = "-"
  var dMsg3 = "-"
  var dMsg4 = "-"
  var dMsg5 = "-"

  // FIXME, eventually we will calculate / fix scaleFactor and boardCfg BasePoint ...
  // ... from window dimensions supplied in main
  var scaleFactor = 1.0

  val hexBoard = HexBoard(boardCfg, scaleFactor)
  
  val pieces = Pieces(
    boardCfg,
    hexBoard
  )

  val highLighter = HighLighter(boardCfg, hexBoard, scaleFactor)

// --- End of hex game intrusion

  def updateModel(
      context: SceneContext[FlicFlacStartupData],
      model: FlicFlacGameModel
  ): GlobalEvent => Outcome[FlicFlacGameModel] = {

    case e: PointerEvent.PointerDown =>
      dMsg1 = "PoDown"
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Down, Pos on Grid
          dMsg2 = "GridTrue"
          pieces.findPieceSelected() match
            case Some(piece) => 
              // Pointer Down, Pos on Grid, Piece Selected
              dMsg3 = "PieceFound"
              if (piece.pCurPos == pos) then
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos=PointerPos
                highLighter.setPos(pos)
                highLighter.shine(true)
                pieces.deselectAllPieces()
                piece.setSelected(true)
                dMsg4 = "SelTrue"
              else 
                // Pointer Down, Pos on Grid, Piece Selected, PiecePos!=PointerPos
                if (hexBoard.isThisHexBlack(pos) == true) then 
                  piece.toggleFlip()
                end if
                piece.setSelected(false)
                highLighter.shine(false)
                piece.setPosition(pos)
                dMsg4 = "SelFalse"
              end if

            case None =>
              // Pointer Down, Pos on Grid, No Piece Selected
              pieces.findPieceByPos(pos) match
                case Some(piece) =>
                  // Pointer Down, Pos on Grid, No Piece Selected, PiecePos=PointerPos
                  dMsg3 = "PieceFound"
                  highLighter.setPos(pos)
                  highLighter.shine(true)
                  pieces.deselectAllPieces()
                  piece.setSelected(true)
                  dMsg4 = "SelTrue"
                case None => 
                  // Pointer Down, Pos on Grid, No Piece Selected, No Piece Found
                  dMsg3 = "PieceNotFound"
                  highLighter.setPos(pos)
                  highLighter.shine(true)
                  pieces.deselectAllPieces()
                  dMsg4 = "SelFalse"
              end match // findPieceByPos
          end match // findPieceSelected
        case None =>
          // Pointer Down, Pos off Grid
          dMsg2 = "GridFalse"
          pieces.findPieceSelected() match
            case Some(piece) =>
              // Pointer Down, Pos off Grid, Piece Selected
              dMsg3 = "PieceFound"
              piece.moveToHome()
              piece.setSelected(false)
              highLighter.shine(false)              
              dMsg4 = "SelFalse"
            case None =>
              // Pointer Down, Pos off Grid, No Piece Selected
              dMsg3 = "PieceNotFound"
              highLighter.shine(false)
              pieces.deselectAllPieces()
              dMsg4 = "SelFalse"
      end match // hexXYCoordsFromDisplayXY
      Outcome(model)

    case e: PointerEvent.PointerUp =>    
      dMsg1 = "PoUp"
      val clickPoint = e.position
      val hexPosn = hexBoard.hexXYCoordsFromDisplayXY(clickPoint, scaleFactor)
      hexPosn match
        case Some(pos) =>
          // Pointer Up, Pos on Grid
          pieces.findPieceSelected() match
            case Some(piece) =>
              // Pointer Up, Pos on Grid, Piece Selected
              if (piece.pCurPos == pos) then
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos==PointerPos
                ;
              else 
                // Pointer Up, Pos on Grid, Piece Selected, PiecePos!=PointerPos
                if (hexBoard.isThisHexBlack(pos) == true) then 
                  piece.toggleFlip()
                end if
                piece.setSelected(false)
                highLighter.shine(false)
                piece.setPosition(pos)                
                dMsg4 = "SelFalse"
                dMsg5 = "Still"

            case None =>
              // Pointer Up, Pos on Grid, No piece selected
              ;
          end match // finPieceSelected
          
        case None =>
          // Pointer Up, Pos off Grid
          pieces.findPieceSelected() match
            case Some(piece) =>
              // Pointer Up, Pos off Grid, Piece Selected
              piece.moveToHome()
              piece.setSelected(false)
              highLighter.shine(false)              
              dMsg4 = "SelFalse"
              dMsg5 = "Still"
            case None => 
              // Pointer Up, Pos off Grid, No piece selected
              highLighter.shine(false)
          end match // findPieceSelected

          
      end match // hexXYCoordsFromDisplayXY      
      Outcome(model)

    case _ => 
      Outcome(model)
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
        pieces.findPieceSelected() match
          case Some(p) =>
            dMsg5 = "Moving"
            println("@@@ PointerEventMove @ " + e.position)
            viewModel.optDragPos = Some(e.position)

          case None =>
            dMsg5 = "Still"
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
    val textGame = TextBox(dMsg1+":"+dMsg2+":"+dMsg3+":"+dMsg4+":"+dMsg5+":"+iFrameTick+":"+iDragTick, 1000, 40)
      .withColor(RGBA.Black)
      .withFontSize(Pixels(30))
      .moveTo(20, 0)
    val dragState = if (viewModel.dragOn) then "Drag:ON" else "DRAG:OFF"
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
                    |+| SceneUpdateFragment(viewModel.paramsButton.draw)
//                  |+| SceneUpdateFragment(viewModel.gameButton.draw)
                    |+| SceneUpdateFragment(viewModel.resultsButton.draw)
                    |+| hexBoard.paint(scaleFactor)
                    |+| highLighter.paint(scaleFactor)
                    |+| pieces.paint(scaleFactor, viewModel.optDragPos)
    )
    

final case class GameSceneViewModel(
  var optDragPos : Option[Point],
  var dragOn : Boolean,
  dragButton: Button,
  splashButton: Button,
  paramsButton: Button,
//  gameButton: Button,
  resultsButton: Button 
):
  def update(mouse: Mouse, pointers: Pointers): Outcome[GameSceneViewModel] =
    for {
      bn0 <- dragButton.updateFromPointers(pointers)
      bn1 <- splashButton.updateFromPointers(pointers)
      bn2 <- paramsButton.updateFromPointers(pointers)
//      bn3 <- gameButton.updateFromPointers(pointers)
      bn4 <- resultsButton.updateFromPointers(pointers)
    } yield this.copy( dragButton = bn0, splashButton = bn1, paramsButton = bn2, /*gameButton = bn3,*/ resultsButton = bn4)

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
        buttonAssets = GameAssets.buttonParamsAssets,
        bounds = Rectangle(20, 220, 240, 80),
        depth = Depth(6)
      ).withUpActions(ButtonParamsEvent),
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
    
