package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network
import game.FlicFlacGameModel.getStartUpStates

object SceneParams extends Scene[FlicFlacStartupData, FlicFlacGameModel, FlicFlacViewModel]:
  type SceneModel = FlicFlacGameModel
  type SceneViewModel = GameSceneViewModel

  def name: SceneName = SceneName("SceneParams")

  def modelLens: Lens[FlicFlacGameModel, FlicFlacGameModel] =
    Lens.keepLatest

  def viewModelLens: Lens[FlicFlacViewModel, SceneViewModel] =
    Lens(
      _.gameScene,
      (m, vm) => m.copy(gameScene = vm)
    )

  def eventFilters: EventFilters = EventFilters.Permissive

//  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set(SSGame("SubSystemPeerJS"))
  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty

  var timerCON3 = TickTimer.stop() // this timer used to transmit initial settings
  
  def updateViewModel(context: SceneContext[FlicFlacStartupData], model: SceneModel, viewModel: SceneViewModel): GlobalEvent => Outcome[SceneViewModel] = {
    e =>
      Outcome(viewModel)
  }

  def updateModel(context: SceneContext[FlicFlacStartupData], model: FlicFlacGameModel): GlobalEvent => Outcome[FlicFlacGameModel] = {
    e =>
      e match
        case e: FlicFlacGameUpdate.Info =>
          scribe.debug("@@@ SceneParams FlicFlacGameUpdate.Info")

          val n1 = context.frameContext.startUpData.flicFlacBootData.n1
          val n2 = context.frameContext.startUpData.flicFlacBootData.n2
          val newModel1 = e.ffgm.copy(ourName = n1, oppoName = n2, ourPieceType = model.ourPieceType)

          if (model.gameState == GameState.START_CON3) then
            if (n1.compare(n2) < 0 ) then 
              // this is the INITIATOR in state START_CON3
              val newModel2 = newModel1.copy( gameState = GameState.START_CON4 )
              timerCON3 = TickTimer.stop()
              FlicFlacGameModel.modify(newModel2, None, None)
            else
              // this is the RESPONDER in state START_CON3 - the ONE AND ONLY time "ourpieceType" may be changed
              val newPieceType = if (model.ourPieceType == CYLINDER) BLOCK else CYLINDER
              val newModel2 = newModel1.copy( gameState = GameState.START_CON4, ourPieceType = newPieceType)
              FlicFlacGameModel.modify(newModel2, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel2))
            end if
          else 
            Outcome(newModel1)
          end if       

        case e: PointerEvent.PointerDown =>
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, "")) // this clears any panel showing

        // Keyboard Interface for testing purposes only ...
        case k: KeyboardEvent.KeyDown =>
          if k.keyCode == Key.F3 then Outcome(model).addGlobalEvents(SceneEvent.Next)
          else Outcome(model).addGlobalEvents(WebRtcEvent.Close) 
          end if

        case FrameTick => 
          val n1 = context.frameContext.startUpData.flicFlacBootData.n1
          val n2 = context.frameContext.startUpData.flicFlacBootData.n2
          val initiator = (n1.compare(n2) < 0)
          
          if (initiator == true) && (model.gameState == GameState.START_CON3) then 
            if (TickTimer.isInactive(timerCON3)) then 
              timerCON3 = TickTimer.start(TT_THREE_SECONDS)
            end if
            if (TickTimer.expired(timerCON3)) then
              timerCON3 = TickTimer.start(TT_THREE_SECONDS)
              Outcome(model).addGlobalEvents(WebRtcEvent.SendData(model))
            else
              Outcome(model)
            end if
          else
            Outcome(model)
          end if

        case _ => Outcome(model)

  }

  def present(context: SceneContext[FlicFlacStartupData], model: SceneModel, viewModel: SceneViewModel): Outcome[SceneUpdateFragment] = {
    val width = GameAssets.GameSceneDimensions.width
    val height = GameAssets.GameSceneDimensions.height
    
    val gs = model.gameState

    val textGameState = TextBox("GameState: " + gs.toString(), 800, 30)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(20))
      .moveTo(200, 50)

    val topXandY = 200
    Outcome (
          SceneUpdateFragment.empty
          // |+| SceneUpdateFragment(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Cyan)).withDepth(Depth(10)))
          |+| SceneUpdateFragment(Shape.Box(Rectangle(topXandY, topXandY, 200, 200), Fill.Color(RGBA.Magenta)).withDepth(Depth(10))) // ..... (A)
          |+| SceneUpdateFragment(Shape.Box(Rectangle(topXandY+4, topXandY+4, 192, 192), Fill.Color(RGBA.White)).withDepth(Depth(11))) // ... (B)
          |+| SceneUpdateFragment(textGameState)
    )
  }



