package game

import indigo.*

/* HexBoardHelper contains additional functions used to test the hex board during development.
 */

class HexBoardHelper(boardCfg: BoardConfig) extends HexBoard(boardCfg: BoardConfig):

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
    if ((width - 1) & 1) == 1 then fillVerticalBorder(centre, 1, height, color)
    else fillVerticalBorder(centre, 0, height, color)
    end if
  end fillVerticalCentreLine

  /*
  fillTopCentreHex sets the top centre hex to a color
  This is a debug/test function
   */
  def fillTopCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillTopCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width - 1) / 2
    if ((width - 1) & 1) == 1 then
      val hh = hexArray(centre)(1)
      hexArray(centre)(1) = HH(
        2 * centre + 1,
        1,
        color,
        hh.q,
        hh.r,
        hh.s,
        hh.xP,
        hh.yP
      ) // tested for 2,4,6
    else
      val hh = hexArray(centre)(0)
      hexArray(centre)(0) = HH(
        2 * centre,
        0,
        color,
        hh.q,
        hh.r,
        hh.s,
        hh.xP,
        hh.yP
      ) // tested for 3,5
    end if
  end fillTopCentreHex

  /*
  fillTopBottomHex sets the bottom centre hex to a color
  This is a debug/test function
   */
  def fillBottomCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillBottomCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width - 1) / 2
    if ((width - 1) & 1) == 1 then
      val hh = hexArray(centre)(height - 1)
      hexArray(centre)(height - 1) = HH(
        2 * centre + 1,
        height - 1,
        color,
        hh.q,
        hh.r,
        hh.s,
        hh.xP,
        hh.yP
      ) // even size
    else
      val hh = hexArray(centre)(height - 2)
      hexArray(centre)(height - 2) = HH(
        2 * centre,
        height - 2,
        color,
        hh.q,
        hh.r,
        hh.s,
        hh.xP,
        hh.yP
      ) // odd size
    end if
  end fillBottomCentreHex

  /*
  fillCentreHex sets the central hex to a color
  This is a debug/test function
   */
  def fillCentreHex(width: Int, height: Int, color: Int): Unit =
    //  println("fillCentreHex width:" + width + " height:" + height + " color:" + color)
    val x: Int = (width - 1) / 2
    val y: Int = height / 2
    if ((width - 1) & 1) == 1 then
      val hh = hexArray(x)(y)
      hexArray(x)(y) = HH(2 * x + 1, y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP) // even size
    else
      val hh = hexArray(x)(y - 1)
      hexArray(x)(y - 1) = HH(2 * x, y - 1, color, hh.q, hh.r, hh.s, hh.xP, hh.yP) // odd size
    end if
  end fillCentreHex
end HexBoardHelper
