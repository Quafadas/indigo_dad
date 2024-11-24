package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class FlicFlacPlayerParams(
    playPams1_OurName: String, // ......... default "OurName"
    playPams2_OppoName: String, // ........ default "OppoName"
    playPams3_ScoreToWin: Int, // ......... default 11
    playPams4_TurnTime: Int, // ........... default 10 seconds
    playPams5_CaptorsTime: Int, // ........ default 5 seconds
    playPams6_RandEventProb: Int // ....... default 1 (in 100)
) derives Encoder.AsObject,
      Decoder

object FlicFlacPlayerParams:

  def getParams(startupData: FlicFlacStartupData): FlicFlacPlayerParams =

    val name1 = startupData.flicFlacBootData.name1
    val name2 = startupData.flicFlacBootData.name2

    val cacheConfigOrDefault =
      decode[FlicFlacPlayerParams](org.scalajs.dom.window.localStorage.getItem("FlicFlac-Params")) match
        case Right(playerParams: FlicFlacPlayerParams) =>
          if (name1.compare(name2) < 0) then
            // we are the PeerJS initiator
            playerParams
          else
            // we are the PeerJS responder
            playerParams.copy(playPams1_OurName = name2, playPams2_OppoName = name1)
          end if

        case Left(_) =>
          scribe.error("@@@ FlicFlacPlayerParams getParams failed - assert default")
          FlicFlacPlayerParams(
            "Player1", // ..... Our name
            "Player2", // ..... Opponents name
            11, // ............ ScoreToWin
            10, // ............ TurnTime
            5, // ............. CaptorsTime
            1 // .............. cfgRandEventProb
          )
    scribe.debug("@@@ FlicFlacPlayerParams getParams " + cacheConfigOrDefault)
    cacheConfigOrDefault
  end getParams

end FlicFlacPlayerParams
