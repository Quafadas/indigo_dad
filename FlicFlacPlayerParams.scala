package game

import indigo.*
import io.circe.Encoder
import io.circe.Decoder
import io.circe.syntax.*
import io.circe.parser.decode

final case class FlicFlacPlayerParams(
  playPamsLocalIpAddr: String, // ..... default 127.0.0.1
  playPamsLocalPort: Int, // .......... default 3000
  playPamsLocalShape: Int, // ..... ... default 0 = cylinder
  playPamsRemoteIpAddr: String, // .... default 127.0.0.1
  playPamsRemotePort: Int, // ......... default 3000
  playPamsRemoteShape: Int, // ........ default 1 = block
  playPamsScoreToWin: Int, // ......... default 11
  playPamsTurnTime: Int, // ........... default 10 seconds
  playPamsCaptorsTime: Int, // ........ default 5 seconds
  playPamsRandEventProb: Int, // ...... default 1 (in 100)
) derives Encoder.AsObject,
    Decoder

object FlicFlacPlayerParams:

  def retrieve(): FlicFlacPlayerParams = 
    val cacheConfigOrDefault = decode[FlicFlacPlayerParams](org.scalajs.dom.window.localStorage.getItem("FlicFlacPlayerParams")) match
      case Right(playerParams: FlicFlacPlayerParams) =>
        scribe.debug("@@@ Retrieved PlayerParams")
        playerParams
      case Left(_)=> 
        scribe.error("@@@ Retrieve PlayerParams failed - assert default")
        FlicFlacPlayerParams (
          "127.0.0.1", //.... LocalIpAddr
          3000, // .......... LocalPort 
          CYLINDER, // ...... LocalShape
          "127.0.0.1", // ... RemoteIpAddr
          3000, // .......... RemotePort
          BLOCK, // ......... RemoteShape
          11, // ............ ScoreToWin
          10, // ............ TurnTime 
          5, // ............. CaptorsTime
          1 // .............. cfgRandEventProb
        )
    cacheConfigOrDefault
  end retrieve
