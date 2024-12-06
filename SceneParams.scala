package game

import indigo.*
import indigo.scenes.*
import indigoextras.ui.*
import io.circe.syntax.*
import io.circe.parser.decode
import indigo.shared.events.KeyboardEvent.KeyUp
import indigo.platform.networking.Network
import game.SceneParams.lastTxGameModel

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

  var lastTxGameModel: Option[FlicFlacGameModel] = None
  val RETRANSMIT_TIMEOUT = 50 // 5 seconds
  val INITIAL_TIMEOUT = 70 // 7 seconds
  var timerP1 = TickTimer.start(INITIAL_TIMEOUT) // protocol timer to retranmsit last frame if nn response with RETRANSMIT_TIMEOUT
  
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
          val currentState = if (bInitiator) then model.initiatorGameState else model.responderGameState

          scribe.info("@@@ SceneParams Old: INITIATOR:" + model.initiatorGameState.toString() + " RESPONDER:" + model.responderGameState.toString())

          if (bInitiator == true) then
            val outcome1 = initiatorStateMachine(context, currentState, newModel1)
            lastTxGameModel match
              case Some(t) => 
                scribe.info("@@@ SceneParams New: INITIATOR:" + t.initiatorGameState.toString() + " RESPONDER:" + t.responderGameState.toString())
              case _ =>
                scribe.info("@@@ SceneParams no change detected")
            outcome1            
          else
            val outcome2 = responderStateMachine(context, currentState, newModel1)
            lastTxGameModel match
              case Some(t) => 
                scribe.info("@@@ SceneParams New: INITIATOR:" + t.initiatorGameState.toString() + " RESPONDER:" + t.responderGameState.toString())
              case _ =>
                scribe.info("@@@ SceneParams no change detected")
            outcome2
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

          if (initiator == true) && (model.initiatorGameState == GameState.START_CON3) && (lastTxGameModel == None) then
            timerP1 = TickTimer.start(RETRANSMIT_TIMEOUT)
            lastTxGameModel = Some(model)
            Outcome(model).addGlobalEvents(WebRtcEvent.SendData(model))
          else if (TickTimer.expired(timerP1)) then
            timerP1 = TickTimer.start(RETRANSMIT_TIMEOUT) // restart the tx retransmission timeout
            lastTxGameModel match
              case Some(lastTxModel) =>             
                scribe.debug("@@@ TimerP1 expired so retransmission")
                Outcome(model).addGlobalEvents(WebRtcEvent.SendData(lastTxModel))
              case None =>
                scribe.debug("@@@ TimerP1 expired but nothing to transmit")
                Outcome(model)
          else
            Outcome(model)
          end if
          
        case _ => 
          Outcome(model)

  }

  def present(context: SceneContext[FlicFlacStartupData], model: SceneModel, viewModel: SceneViewModel): Outcome[SceneUpdateFragment] = {
    val width = GameAssets.GameSceneDimensions.width
    val height = GameAssets.GameSceneDimensions.height

    val n1 = context.frameContext.startUpData.flicFlacBootData.n1
    val n2 = context.frameContext.startUpData.flicFlacBootData.n2
    val bInitiator = (n1.compare(n2) < 0)
   
    val gs = if (bInitiator == true) then model.initiatorGameState else model.responderGameState

    val textT1 = TextBox("*** FlicFlac BootLoader ***", width,80).withColor(RGBA.Yellow).withFontSize(Pixels(60)).alignCenter.moveTo(0,0)

    val topX = 10
    val topY = 90
    val fontWidth = 13
    val nameWidth = (Math.max(n1.length, n2.length)+2) * fontWidth  // we add a plus 2 for the : and safety
    val nameHeight = 30
    val nameGap = 10
    val boxGap = 10

    val textN1 = TextBox(n1 + ":", nameWidth, nameHeight).withColor(RGBA.Yellow).withFontSize(Pixels(20)).alignRight.moveTo(topX, topY)
    val textN2 = TextBox(n2 + ":", nameWidth, nameHeight).withColor(RGBA.Yellow).withFontSize(Pixels(20)).alignRight.moveTo(topX, topY+nameHeight+nameGap)
    val boxB1 = Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow)).moveTo(topX+nameWidth+boxGap, topY)
    val boxB2 = Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow)).moveTo(topX+nameWidth+boxGap, topY+nameHeight+nameGap)
    val boxB3 = Shape.Box(Rectangle(0, 0, 24, 24), Fill.Color(RGBA.Yellow)).moveTo(topX+nameWidth+boxGap + 24 + nameGap, topY+nameHeight+nameGap)

    val initiatorProgress = Math.min(model.initiatorGameState.ordinal+1,8)
    val responderProgress = Math.min(model.responderGameState.ordinal+1,8)

    var initiatorBoxes = Layer.empty
    for (i <- 1 to initiatorProgress)
      val x = topX + nameWidth + boxGap + boxGap + ((24 + boxGap) * (i-1))
      val y = topY
      initiatorBoxes = initiatorBoxes |+| Layer.Content(boxB1.moveTo(x,y))

    var responderBoxes = Layer.empty
    for (i <- 1 to responderProgress)
      val x = topX + nameWidth + boxGap + boxGap + ((24 + boxGap) * (i-1))
      val y = topY + nameHeight + nameGap
      responderBoxes = responderBoxes |+| Layer.Content(boxB1.moveTo(x,y))


    Outcome (
          SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Shape.Box(Rectangle(0, 0, width, height), Fill.Color(RGBA.Black))))
          |+| SceneUpdateFragment(LayerKeys.Middleground -> Layer.Content(Batch(textT1, textN1, textN2)))
          |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> initiatorBoxes)
          |+| SceneUpdateFragment(LayerKeys.ForegroundPieces -> responderBoxes)
    )
  }

  def initiatorStateMachine(  context: SceneContext[FlicFlacStartupData], currentState: GameState, responderModel: FlicFlacGameModel) : Outcome[FlicFlacGameModel] = 
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
        lastTxGameModel = Some(newModel)
        FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))

      case GameState.START_CON5 => 
        if (currentState == GameState.START_CON4) then
          scribe.info("@@@ SceneParams INITIATOR transitions to START_CON5")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON5)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if

      case GameState.START_CON6 => 
        if (currentState == GameState.START_CON5) then
          scribe.info("@@@ SceneParams INITIATOR transitions to START_CON6")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON6)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if

      case GameState.START_CON7 => 
        if (currentState == GameState.START_CON6) then
          scribe.info("@@@ SceneParams INITIATOR transitions to START_CON7")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON7)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if

      case GameState.START_CON8 => 
        if (currentState == GameState.START_CON7) then
          scribe.info("@@@ SceneParams INITIATOR transitions to START_CON8")
          val newModel = responderModel.copy(initiatorGameState = GameState.START_CON8)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams INITIATOR stuck at " + currentState.toString())
          val newModel = responderModel.copy(initiatorGameState = currentState)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if

      case GameState.CYLINDER_TURN => 
        scribe.info("@@@ SceneParams INITIATOR invokes SceneGame")
        val newModel = responderModel.copy(gameState = GameState.CYLINDER_TURN, initiatorGameState = GameState.CYLINDER_TURN)
        lastTxGameModel = Some(newModel)
        timerP1 = TickTimer.stop()
        FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(SceneEvent.Next)
        
      case _ =>
        val sError = "SceneParams INITIATOR error " + responderModel.initiatorGameState.toString() + " and " + responderModel.responderGameState.toString()
        scribe.debug("@@@ " + sError)
        val newModel = responderModel
        lastTxGameModel = Some(newModel)
        FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, sError))
  end initiatorStateMachine
      
  def responderStateMachine(  context: SceneContext[FlicFlacStartupData], currentState: GameState, initiatorModel: FlicFlacGameModel) : Outcome[FlicFlacGameModel] = 
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
        lastTxGameModel = Some(newModel)
        FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))

      case GameState.START_CON4 => 
        if (currentState == GameState.START_CON4) then
          scribe.info("@@@ SceneParams RESPONDER transitions to START_CON5")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON5)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if 

      case GameState.START_CON5 => 
        if (currentState == GameState.START_CON5) then
          scribe.info("@@@ SceneParams RESPONDER transitions to START_CON6")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON6)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if 

      case GameState.START_CON6 => 
        if (currentState == GameState.START_CON6) then
          scribe.info("@@@ SceneParams RESPONDER transitions to START_CON7")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON7)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if 

      case GameState.START_CON7 => 
        if (currentState == GameState.START_CON7) then
          scribe.info("@@@ SceneParams RESPONDER transitions to START_CON8")
          val newModel = initiatorModel.copy(responderGameState = GameState.START_CON8)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        else
          scribe.info("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if 

      case GameState.START_CON8 => 
        if (currentState == GameState.START_CON8) then
          scribe.info("@@@ SceneParams RESPONDER invokes SceneGame")
          val newModel = initiatorModel.copy(responderGameState = GameState.CYLINDER_TURN, gameState = GameState.CYLINDER_TURN)
          lastTxGameModel = Some(newModel)
          timerP1 = TickTimer.stop()
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel)).addGlobalEvents(SceneEvent.Next)
        else
          scribe.info("@@@ SceneParams RESPONDER stuck at " + currentState.toString())
          val newModel = initiatorModel.copy(responderGameState = currentState)
          lastTxGameModel = Some(newModel)
          FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(WebRtcEvent.SendData(newModel))
        end if 

      case GameState.CYLINDER_TURN => 
        scribe.info("@@@ SceneParams RESPONDER detects inbound CYLINDER_TURN ???")
        val newModel = initiatorModel.copy(responderGameState = currentState)
        lastTxGameModel = Some(newModel)
        Outcome(newModel)

      case _ =>
        val sError = "SceneParams RESPONDER error " + initiatorModel.initiatorGameState.toString() + " and " + initiatorModel.responderGameState.toString()
        scribe.debug("@@@ " + sError)
        val newModel = initiatorModel
        lastTxGameModel = Some(newModel)
        FlicFlacGameModel.modify(newModel, None, None).addGlobalEvents(Freeze.PanelContent(PanelType.P_ERROR, sError))
        
  end responderStateMachine
end SceneParams





  

