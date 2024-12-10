package game

import indigo.*

final case class TurnTimer(
    val iTotalTurnTime: Int, // .......... the turn time in seconds as configured by Params
    val iCaptorsTurnTime: Int, // ........ the captors turn time in seconds as configured by Params
    val iThisTurnTime: Int = 0, // ....... the current time in 10ths of a second
    val iThisTurnExpires: Int = 0 // ..... the future time in 10ths of a second when turn expires
)

object TurnTimer:
  def restartForTurn(tt: TurnTimer): TurnTimer =
    val t2 = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
    val t3 = t2 + (10 * tt.iTotalTurnTime)
    tt.copy(iThisTurnTime = t2, iThisTurnExpires = t3)
  end restartForTurn

  def restartForCaptors(tt: TurnTimer): TurnTimer =
    val t2 = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
    val t3 = t2 + (10 * tt.iCaptorsTurnTime)
    tt.copy(iThisTurnTime = t2, iThisTurnExpires = t3)
  end restartForCaptors

  def update(tt: TurnTimer): Option[TurnTimer] =
    val t2 = math.round(System.currentTimeMillis / 100).toInt // this is 10ths of a second
    if t2 > tt.iThisTurnTime then Some(tt.copy(iThisTurnTime = t2))
    else None
    end if
  end update

  def isActive(tt: TurnTimer): Boolean =
    if (tt.iThisTurnTime == 0) && (tt.iThisTurnExpires == 0) then 
      false
    else
      true
    end if
  end isActive

  def expired(tt: TurnTimer): Boolean =
    if (tt.iThisTurnTime == 0) || (tt.iThisTurnExpires == 0) then
      false
    else if (tt.iThisTurnTime >= tt.iThisTurnExpires) then
      true
    else
      false
    end if
  end expired

  def show(model: FlicFlacGameModel): Layer =
    // all measurements before scaling ...
    // for 0% ...
    // cap part is 25 high ...... and starts-ends at (70-95)
    // body part is 1195 high ... and starts-ends at (95-1220)
    // for 100% ...
    // cap part is 25 high ...... and starts-ends at (1170-1195)
    // body part is 25 high ..... and starts-ends at (1195-1220)

    // 0% = 70 ........ for cap top
    // 100% = 1170 .... for cap top
    // Body-Bottom length is 1170 (=1220 -25 -25) 25 for top and 25 for bottom
    // For N% ...
    // Cap Top(T) = 70 + N * 1170
    // White Rectangle Height = Cap Top -1

    val tt = model.turnTimer
    val iTotalTime = tt.iTotalTurnTime * 10 // total turn time allowed in 10ths of seconds
    val iTurnTime = tt.iThisTurnTime
    val iTurnExpires = tt.iThisTurnExpires

    val iTimeRemaining = math.max(0, iTurnExpires - iTurnTime)
    val iTimeSpent = iTotalTime - iTimeRemaining

    val T: Double = ((1170 * iTimeSpent) / iTotalTime) + 70

    val dSF = hexBoard4.scalingFactor
    val scalableX = GameAssets.GameSceneDimensions.right - hexBoard4.pBase.x - 120
    val iSliderXPos = (math.round(scalableX * dSF)).toInt + hexBoard4.pBase.x
    val iBodyTop = (math.round(95 * dSF)).toInt
    val iCapTop = (math.round(T * dSF)).toInt
    val iWidth = (math.round(52 * dSF)).toInt // changed from 50 to 52 to eliminate sporadic veritcal lines

    val bCylinder = (model.gameState == GameState.CYLINDER_TURN) && (model.ourPieceType == CYLINDER)
    val bBlock = (model.gameState == GameState.BLOCK_TURN) && (model.ourPieceType == BLOCK)
    
    val content1 = 
      if (bCylinder == true) || (bBlock == true) then
        Layer.Content(GameAssets.gTimeSliderActiveBody(dSF).moveTo(iSliderXPos, iBodyTop))
      else
        Layer.Content(GameAssets.gTimeSliderInactiveBody(dSF).moveTo(iSliderXPos, iBodyTop))
      end if

    val content2 = 
      if (bCylinder == true) || (bBlock == true) then     
        Layer.Content(GameAssets.gTimeSliderActiveTop(dSF).moveTo(iSliderXPos, iCapTop))
      else
        Layer.Content(GameAssets.gTimeSliderInactiveTop(dSF).moveTo(iSliderXPos, iCapTop))
      end if

    val r3 = Rectangle(iSliderXPos, 0, iWidth, iCapTop)
    val content3 = Layer.Content(Shape.Box(r3, Fill.Color(RGBA.White)))
    val content4 = content1 |+| content2 |+| content3

    content4
  end show

end TurnTimer
//          |+| SceneUpdateFragment(LayerKeys.Background -> Layer.Content(Batch(textGameState)))
