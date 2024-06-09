package game

import indigo.*

final case class Shape (
    CYLINDER : Boolean = false,
    BLOCK :    Boolean = true
)

// First 6 colors are used in modulo 6 fashion for pieces
val CB = 0                                    // CB for Blue
val CG = 1                                    // CR for Red
val CY = 2                                    // CG for Green
val CO = 3                                    // CY for Yellow
val CR = 4                                    // CO for Orange
val CP = 5                                    // CP for Purple
//-----------------------------------------------------
val CK = 6                                    // CK for Black
val CW = 7                                    // CW for White
val CC = 8                                    // CC for Cyan is a test color
val CM = 9                                    // CM for Magenta is a test color
val CX = 10                                   // CX indicates hex does not so not visible (indicated by transparency field = 0)

def mix(i : Int): RGBA = {
    i match       
      case CX => RGBA(0,0,0,0) 
      case CB => RGBA.Blue     
      case CR => RGBA.Red      
      case CG => RGBA.Green    
      case CY => RGBA.Yellow   
      case CO => RGBA.Orange   
      case CP => RGBA.Purple   
      case CK => RGBA.Black    
      case CW => RGBA.White    
      case CC => RGBA.Cyan     
      case CM => RGBA.Magenta  
      case _ => RGBA.Magenta   
}


val pieceNames : Array[String] = Array(
    "Blue",
    "Red",
    "Green",
    "Yellow",
    "Orange",
    "Purple"
)

class Piece(sh:Shape, hp:Point, pi:Int) {
    val shape:      Shape = sh          // true=block, false=cylinder
    val homePos:    Point = hp          // home position (in hexArray coords)
    var curPos:     Point = hp          // current position (in hexArrayCoords)
    val id:         Int   = pi % 6      // piece Id (modulo 6 ... 0,1,2,3,4,5)
    var fl:         Boolean = false     // piece is flipped (or not)
    var ca:         Boolean = false     // piece is captured (or not)
}



