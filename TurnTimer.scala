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

  def expired(tt: TurnTimer): Boolean =
    (tt.iThisTurnTime >= tt.iThisTurnExpires)
  end expired

  def show(model: FlicFlacGameModel): SceneUpdateFragment =
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

    val dSF = model.scalingFactor
    val scalableX = GameAssets.GameSceneDimensions.right - model.hexBoard3.pBase.x - 120
    val iSliderXPos = (math.round(scalableX * dSF)).toInt + model.hexBoard3.pBase.x
    val iBodyTop = (math.round(95 * dSF)).toInt
    val iCapTop = (math.round(T * dSF)).toInt
    val iWidth = (math.round(50 * dSF)).toInt

    val frag1 = SceneUpdateFragment(GameAssets.gTimeSliderBody(dSF).moveTo(iSliderXPos, iBodyTop))
    val frag2 = SceneUpdateFragment(GameAssets.gTimeSliderTop(dSF).moveTo(iSliderXPos, iCapTop))
    val r3 = Rectangle(iSliderXPos, 0, iWidth, iCapTop)
    val frag3 = SceneUpdateFragment(Shape.Box(r3, Fill.Color(RGBA.White)))

    frag1 |+| frag2 |+| frag3
  end show

end TurnTimer
