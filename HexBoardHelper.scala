package game

import indigo.*

/* HexBoardHelper contains additional functions used to test the hex board during development.
 */

class HexBoardHelper(boardCfg: BoardConfig, fS : Double) 
    extends HexBoard(boardCfg: BoardConfig, fS : Double) :

  fillCentreCentreHex(arrayWidth, arrayHeight, CM)
  fillCentreTopHex(arrayWidth, arrayHeight, CM)
  fillCentreBottomHex(arrayWidth, arrayHeight, CM)
  fillLeftCentreHex(arrayWidth, arrayHeight, CM)
  fillRightCentreHex(arrayWidth, arrayHeight, CM)
  
  /*
  printBoard is prints the details of each hexagon on a new line
  once a complete row is printed, an additional line of "---"
  is printed to indicate the start of a new row
  This is a debug/test funtion
   */

  def printBoard(width: Int, height: Int): Unit =
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        println("HH: " + hexArray(x)(y))
        x += 1
      end while
      println("---")
      y += 1
    end while
  end printBoard

  /*
  fillVerticalCentreLine fills the vertical centre line with a color
  This is a debug/test function
   */
  def fillVerticalCentreLine(width: Int, height: Int, color: Int): Unit =
    //  println("fillVerticalCentreLine width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width - 1) / 2
    if ((width - 1) & 1) == 1 then 
      fillVerticalBorder(centre, 1, height, color)
    else 
      fillVerticalBorder(centre, 0, height, color)
    end if
  end fillVerticalCentreLine

  /*
  fillTopCentreHex sets the NORTH hex to a color
  This is a debug/test function
   */
  def fillCentreTopHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillTopCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width - 1) / 2
    if ((width - 1) & 1) == 1 then
      setHexColor(Point(centre,1),color)
    else
      setHexColor(Point(centre,0),color)
    end if
  end fillCentreTopHex

  /*
  fillTopBottomHex sets the SOUTH hex to a color
  This is a debug/test function
   */
  def fillCentreBottomHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillBottomCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width - 1) / 2
    if ((width - 1) & 1) == 1 then
      setHexColor(Point(centre,height-1),color)
    else
      setHexColor(Point(centre,height-2),color)
    end if
  end fillCentreBottomHex

  /*
  fillCentreCentreHex sets the central hex to a color
  This is a debug/test function
   */
  def fillCentreCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillCentreHex width:" + width + " height:" + height + " color:" + color)
    val x: Int = (width - 1) / 2
    val y: Int = height / 2
    if ((width - 1) & 1) == 1 then
      setHexColor(Point(x,y),color)
    else
      setHexColor(Point(x,y-1),color)
    end if
  end fillCentreCentreHex

  /*
  fillLeftCentreHex sets WEST hex to a color
  This is a debug/test function
   */
  def fillLeftCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillCentreHex width:" + width + " height:" + height + " color:" + color)
    val x: Int = 0
    val y: Int = height / 2
    if ((width - 1) & 1) == 1 then
      setHexColor(Point(x,y),color)
    else
      setHexColor(Point(x,y-1),color)
    end if
  end fillLeftCentreHex

  /*
  fillRightCentreHex sets EAST hex to a color
  This is a debug/test function
   */
  def fillRightCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillCentreHex width:" + width + " height:" + height + " color:" + color)
    val x: Int = width - 1
    val y: Int = height / 2
    if ((width - 1) & 1) == 1 then
      setHexColor(Point(x,y),color)
    else
      setHexColor(Point(x,y-1),color)
    end if
  end fillRightCentreHex  

end HexBoardHelper
