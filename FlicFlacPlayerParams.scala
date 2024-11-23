package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class FlicFlacPlayerParams(
    playPamsOurName: String, // ......... default "OurName"
    playPamsOppoName: String, // ........ default "OppoName"
    playPamsScoreToWin: Int, // ......... default 11
    playPamsTurnTime: Int, // ........... default 10 seconds
    playPamsCaptorsTime: Int, // ........ default 5 seconds
    playPamsRandEventProb: Int // ....... default 1 (in 100)
) derives Encoder.AsObject,
      Decoder

object FlicFlacPlayerParams:

  def getParams(startupData: FlicFlacStartupData): FlicFlacPlayerParams =
    val ourName = startupData.flicFlacBootData.name1
    val oppoName = startupData.flicFlacBootData.name2
    val storageName = 
      if (ourName.compare(oppoName) < 0) then
        // we are the PeerJS initiator
        "FlicFlacPlayerParams1"
      else
        // we are the PeerJS responder
        "FlicFlacPlayerParams2"
      end if

    val cacheConfigOrDefault =
      decode[FlicFlacPlayerParams](org.scalajs.dom.window.localStorage.getItem(storageName)) match
        case Right(playerParams: FlicFlacPlayerParams) =>
          scribe.debug(s"@@@ FlicFlacPlayerParams getParams $playerParams")
          playerParams
        case Left(_) =>
          scribe.error("@@@ FlicFlacPlayerParams getParams failed - assert default")
          FlicFlacPlayerParams(
            "OurName", // ..... Our name
            "OppoName", // .... Opponents name
            11, // ............ ScoreToWin
            10, // ............ TurnTime
            5, // ............. CaptorsTime
            1 // .............. cfgRandEventProb
          )
    cacheConfigOrDefault
  end getParams

end FlicFlacPlayerParams
