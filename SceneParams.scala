package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network

/* 
  INTIATOR criteria .... ourName < oppoName
  RESPONDER criteria ... ourName > oppoName
  NB index1.html prohibits ourName == oppoName
  
  SceneParams and SubSystemPeerJs implement a simple state machine as follows:

  START_CON1 ...... both INITIATOR and RESPONDER generate a peerJS
  START_CON2 ...... both INITIATOR and RESPONDER peerJS established, INITIATOR (repeatedly) issues request to connect
  START_CON3 ...... both INITIATOR and RESPONDER independently confirm connection 
  ................. INITIATOR sends proposed parameters and transitions to CON4 (triggered by peerJS)
  MsgA: Initiator===>>>Responder "proposed params"
  ................. RESPONDER averages game parameters and responds with modified params and transitions to CON4
  MsgB: Initiator<<<===Responder "modified params"
  START_CON4 ...... INITIATOR transmits modifed params with state CYLINDER_TURN, transitions to CYLINDER_TURN
  MsgC: Initiator===>>>Responder "modifed params"
  ................. RESPONDER transitions to CYLINDER_TURN
  CYLINDER_TURN ... both INITIATOR and RESPONDER display hexboard and the game starts
*/

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

  val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set.empty
  //val subSystems: Set[SubSystem[FlicFlacGameModel]] = Set(SSParams("InitMsgFromSceneParams"))

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
          val bInitiator = (n1.compare(n2) < 0)

          scribe.info("@@@ SceneParams Old: INITIATOR:" + model.initiatorGameState.toString() + " RESPONDER:" + model.responderGameState.toString)
          scribe.info("@@@ SceneParams New: INITIATOR:" + newModel1.initiatorGameState.toString() + " RESPONDER:" + newModel1.responderGameState.toString)

          if (bInitiator == true) then
            initiatorStateMachine(context, newModel1)
          else
            responderStateMachine(context, newModel1)
          end if

        case e: PointerEvent.PointerDown =>
          Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_INVISIBLE, "")) // this clears any panel showing

        // Keyboard Interface for testing purposes only ...
        case k: KeyboardEvent.KeyDown =>
          if k.keyCode == Key.F3 then Outcome(model).addGlobalEvents(SceneEvent.Next)
          else if k.keyCode == Key.F4 then Outcome(model).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR,"Test Error from PARAMS FKEY_F4"))
          else Outcome(model)
          end if

        case FrameTick => 
          val n1 = context.frameContext.startUpData.flicFlacBootData.n1
          val n2 = context.frameContext.startUpData.flicFlacBootData.n2
          val initiator = (n1.compare(n2) < 0)
          
          if (initiator == true) && (model.initiatorGameState == GameState.START_CON3) then 
            if (TickTimer.isInactive(timerCON3)) then 
              timerCON3 = TickTimer.start(TT_THREE_SECONDS)
            end if
            if (TickTimer.expired(timerCON3)) then
              scribe.debug("@@@ TimerCON3 triggered")
              timerCON3 = TickTimer.start(TT_THREE_SECONDS)
              Outcome(model).addGlobalEvents(WebRtcEvent.SendData(model))
            else
              Outcome(model)
            end if
          else
            timerCON3 = TickTimer.stop()            
            Outcome(model)
          end if

        case _ => Outcome(model)

  }

  def present(context: SceneContext[FlicFlacStartupData], model: SceneModel, viewModel: SceneViewModel): Outcome[SceneUpdateFragment] = {
    val width = GameAssets.GameSceneDimensions.width
    val height = GameAssets.GameSceneDimensions.height

    val n1 = context.frameContext.startUpData.flicFlacBootData.n1
    val n2 = context.frameContext.startUpData.flicFlacBootData.n2
    val bInitiator = (n1.compare(n2) < 0)
   
    val gs = if (bInitiator == true) then model.initiatorGameState else model.responderGameState

    val textGameState = TextBox("GameState: " + gs.toString(), 800, 30)
      .withColor(RGBA.Yellow)
      .withFontSize(Pixels(20))
      .moveTo(200, 50)

    val topXandY = 200
    val testMsg = TextBox("Hello World", 800, 30).withColor(RGBA.Blue).withFontSize(Pixels(20)).moveTo(topXandY+20,topXandY+20)
    val boxMagenta = Shape.Box(Rectangle(topXandY, topXandY, 200, 200), Fill.Color(RGBA.Magenta)) // ...... (A)
    val boxWhite = Shape.Box(Rectangle(topXandY+4, topXandY+4, 192, 192), Fill.Color(RGBA.White)) // .... (B)

    Outcome (
          SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Batch(boxMagenta, boxWhite)))
          |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Batch(textGameState)))
          |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Batch(testMsg)))
    )
  }

def initiatorStateMachine(  context: SceneContext[FlicFlacStartupData], responderModel: FlicFlacGameModel) : Outcome[FlicFlacGameModel] = 
  responderModel.responderGameState match
    case GameState.START_CON1 => 
      Outcome(responderModel)
    case GameState.START_CON2 => 
      Outcome(responderModel)
    case GameState.START_CON3 => 
      Outcome(responderModel)
    case GameState.START_CON4 => 
      scribe.info("@@@ SceneParams INITIATOR transitions to START_CON4")
      val newModel = responderModel.copy(initiatorGameState = GameState.START_CON4)
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
    case GameState.CYLINDER_TURN => 
      scribe.info("@@@ SceneParams INITIATOR invokes SceneGame")
      val newModel = responderModel.copy(gameState = GameState.CYLINDER_TURN, initiatorGameState = GameState.CYLINDER_TURN)
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(SceneEvent.Next)
    case _ =>
      val sError = "SceneParams INITIATOR error " + responderModel.initiatorGameState.toString() + " and " + responderModel.responderGameState.toString()
      scribe.debug("@@@ " + sError)
      val newModel = responderModel
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, sError))
end initiatorStateMachine
    
def responderStateMachine(  context: SceneContext[FlicFlacStartupData], initiatorModel: FlicFlacGameModel) : Outcome[FlicFlacGameModel] = 
  initiatorModel.initiatorGameState match
    case GameState.START_CON1 => 
      Outcome(initiatorModel)
    case GameState.START_CON2 => 
      Outcome(initiatorModel)
    case GameState.START_CON3 => 
      scribe.info("@@@ SceneParams RESPONDER transitions to START_CON4")
      // this is the RESPONDER - the ONE AND ONLY time the params marked with ### may be changed
      val newPieceType = if (initiatorModel.ourPieceType == CYLINDER) BLOCK else CYLINDER
      val originalPlayerParams = FlicFlacPlayerParams.getParams(context.frameContext.startUpData)              
      val newScoreToWin = (initiatorModel.winningScore + originalPlayerParams.playPams3_ScoreToWin)/2
      val newTurnTimer = (initiatorModel.turnTimer.iTotalTurnTime + originalPlayerParams.playPams4_TurnTime)/2
      val newCaptorsTimer = (initiatorModel.turnTimer.iCaptorsTurnTime + originalPlayerParams.playPams5_CaptorsTime)/2
      val newRandEventFreq = (initiatorModel.randEventFreq + originalPlayerParams.playPams6_RandEventProb)/2

      val newModel = initiatorModel.copy(
                        responderGameState = GameState.START_CON4,
                        ourPieceType = newPieceType, // .............................###
                        winningScore = newScoreToWin, // ............................###
                        turnTimer = TurnTimer(newTurnTimer, newCaptorsTimer), // ....###
                        randEventFreq = newRandEventFreq // .........................###
                        )
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
    case GameState.START_CON4 => 
      scribe.info("@@@ SceneParams RESPONDER invokes SceneGame")
      val newModel = initiatorModel.copy(responderGameState = GameState.CYLINDER_TURN, gameState = GameState.CYLINDER_TURN)
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(SceneEvent.Next)      
    case GameState.CYLINDER_TURN => 
      Outcome(initiatorModel)
    case _ =>
      val sError = "SceneParams RESPONDER error " + initiatorModel.initiatorGameState.toString() + " and " + initiatorModel.responderGameState.toString()
      scribe.debug("@@@ " + sError)
      val newModel = initiatorModel
      FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, sError))
      
end responderStateMachine






  

