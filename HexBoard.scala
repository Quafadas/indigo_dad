package game

import indigo.*

/*  class HH is the class behind each individual hex cell in the grid
    including the ones you cannot see.
 */

case class HH(
    x: Int, // cartesian x coordinate of centre of hex
    y: Int, // cartesian y coordinate of centre of hex
    c: Int, // colour and visibility of hex
    q: Int, // cubic q Coord (-ve to +ve = left to right)
    r: Int, // cubic r Coord (-ve to +ve = top right to bottom left)
    s: Int, // cubic s Coord (-ve to +ve = bottom right to top left)
    xP: Int, // pixel x offset from base point for origin of png to paint this hex
    yP: Int // pixel y offset from base point for origin of png to paint this hex
)

class HexBoard(boardCfg: BoardConfig, initScale : Double):

  /** ******************* The Hex Board ***
    *
    * The implementation of this hexagonal grid is based on the information kindly supplied at
    * https://www.redblobgames.com/grids/hexagons/ Thankou! to Red Blob Games
    *
    * Please refer to my Documentation/HexInfo.pdf for a full description of the construction of the hexagonal grid
    *
    * This grid is "flat-top orientated" and uses a primary "cartesian coordinate" system (xCoord,yCoord) of "odd-q" for
    * display purposes. This grid also carries a secondary "cube coordinate" system (qCoord, rCoord, sCoord) for
    * purposes of the gaming model
    *
    * The rows of the hexArray are interleaved with the odd rows being pushed down half a hexagon, eg the start of the
    * first four rows of the hexArray grid looks like this (so this is the top left hand corner with structure
    * abbrievated)...
    *
    * HH(0,0,c,0,0,0,xP,yP), HH(2,0,c,2,-1,-1,xP,yP), HH(4,0,c,4,-2,-2,xP,yP), HH(6,0,c,6,-3,-3,xP,yP)
    * * * * * * * HH(1,1,c,1,0,-1,xP,yP), HH(3,1,c,3,-1,-2,xP,yP), HH(5,1,c,5,-2,-3,xP,yP), HH(7,1,c,7,-3,-4,xP,yP)
    * HH(0,2,c,0,1,-1,xP,yP), HH(2,2,c,2,0,-2,xP,yP), HH(4,2,c,4,-1,-3,xP,yP), HH(6,2,c,6,-2,-4,xP,yP)
    * * * * * * * HH(1,3,c,1,1,-2,xP,yP), HH(3,3,c,3,0,-3,xP,yP), HH(5,3,c,5,-1,-4,xP,yP), HH(7,3,c,7,-2,-5,xP,yP)
    * HH(0,4,c,0,2,-2,xP,yP), HH(2,4,c,2,1,-3,xP,yP), HH(4,4,c,4,0,-4,xP,yP), HH(6,4,c,6,-1,-5,xP,yP)
    *
    * NB The storage for this snippet would be HexArray(4,5) ... even though the xy coords range from (0,0) to (7,4)
    */

  val gHex = boardCfg.getHexGraphic() // The Hex graphic used to paint the grid

  // The hexagons are aligned such that they have a flat bottom and top
  // The board is aligned such that it has pointy bottom and top, in terms of flat bottom hexagons
  // Note there is an interleaving edge between neighbouring hexagons, that comes from the rows above and below

  var hexBoardUpdated = true // set true when paint needs to recalculate the board image
  var hexFragsCombined = SceneUpdateFragment.empty // holds the latest calculations from the paint routine

  // The number of hexagonal rings (ring of 6 with centre) composing one side of the board
  val sZ = boardCfg.getSideSize() 
  val borderlessArrayWidth = 6 * (sZ - 1) + 3         // 3,9,15,21 ...
  val borderlessArrayHeight = 12 * (sZ - 1) + 6       // 7,17,29,41 ...
  val arrayWidth = (borderlessArrayWidth + 1 + 2) / 2 // +2 because of borders
  val arrayHeight = borderlessArrayHeight + 4         // +4 because of borders

  var hexArray = createArrayOfHH(arrayWidth, arrayHeight)
  // println("hexArray size:" + sZ + " w:h" + borderlessArrayWidth +":" + height + " aw:ah" + + arrayWidth +":" + borderlessArrayHeight + " .rowLength:" + hexArray.length)

  // start with black board, populates q,r,s (for debugging the helper routine printBoard can follow this line)
  fillBoard(arrayWidth, arrayHeight, mix(CK), initScale )

  // The first two rows are an invisible border
  fillHorizontalBorder(0, 2, arrayWidth, CX ) 

  // this is the pattern of the board
  colorBoardHexes(2, arrayWidth, arrayHeight ) 

  // first column is border
  fillVerticalBorder(0, 0, arrayHeight, CX) 

  // last two columns are borders
  fillVerticalBorder(arrayWidth - 1, 0, arrayHeight, CX ) 
  fillVerticalBorder(arrayWidth - 1, 1, arrayHeight, CX)

  // The last two rows are an invisible border
  fillHorizontalBorder(arrayHeight - 2, 2, arrayWidth, CX ) 

  // trim off the four corners (uses q,r,s coords)
  trimBoard( arrayWidth, arrayHeight, CX ) 

  // establish extra hex for the Cylinder's Home
  establishCylinderHome( arrayWidth, arrayHeight, CK)

  // establish extra hex for the Block's Home
  establishBlockHome( arrayWidth, arrayHeight, CK)


  // ########################################################

  /*
  createArrayOfHH creates an array of mini hexagons according to width and height parameters
  NB width and height are hexagonal width/height dimensions, not physical dimensions
   */
  def createArrayOfHH(width: Int, height: Int): Array[Array[HH]] =
    Array.ofDim[HH](width, height)

  /*
  fillBoard populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  after the while loops calculateXpyP calcualtes the paint position
  for the graphic to paint the hex cell
   */
  def fillBoard(width: Int, height: Int, color: RGBA, fS : Double): Unit =
    var row = 0
    while row < height do
      var col = row & 1
      var n = 0
      while n < width do
        val q = col
        val r = (row - col) / 2
        val s = (-row - col) / 2
        hexArray(n)(row) = HH(col, row, CW, q, r, s, 0, 0)
        col += 2
        n += 1
      end while
      row += 1
    end while
    calculateXpYp(fS) // calculate the hex cells paint positions
  end fillBoard

  /*
  colorBoardHexes generates the pattern colors for each mini hex using the
  color combinations in rowTemplate
   */
  def colorBoardHexes(row: Int, arrayWidth: Int, arrayHeight: Int): Unit =

    val rowTemplate: Array[Vector[Int]] = Array(
      Vector(CK, CK, CK), // 0
      Vector(CR, CR, CY), // 1
      Vector(CO, CP, CO), // 2
      Vector(CK, CK, CK), // 3
      Vector(CY, CB, CY), // 4
      Vector(CG, CG, CO), // 5
      Vector(CK, CK, CK), // 6
      Vector(CB, CB, CR), // 7
      Vector(CP, CG, CP), // 8
      Vector(CK, CK, CK), // 9
      Vector(CR, CY, CR), // 10
      Vector(CO, CO, CP), // 11
      Vector(CK, CK, CK), // 12
      Vector(CY, CY, CB), // 13
      Vector(CG, CO, CG), // 14
      Vector(CK, CK, CK), // 15
      Vector(CB, CR, CB), // 16
      Vector(CP, CP, CG)  // 17
    )
    val sZ = boardCfg.getSideSize()
    var col = 0
    var n = 0
    var thisRow = row
    while thisRow < arrayHeight - 2 do
      if (thisRow & 1) == 0 then
        col = 2
        n = 1
      else
        col = 1
        n = 0
      end if

      while n < arrayWidth - 1 do
        val offset =
          (sZ & 1) * 10 // an odd size requires a vertical shift of the hex grid
        val hexColor =
          rowTemplate((thisRow - 2 + offset) % rowTemplate.length)(n % 3)
        val hh = hexArray(n)(thisRow)
        hexArray(n)(thisRow) = HH(col, thisRow, hexColor, hh.q, hh.r, hh.s, hh.xP, hh.yP)
        col += 2
        n += 1
      end while
      thisRow += 1
    end while
  end colorBoardHexes

  /*
  fillHorizontalBorder is used to set the color of one or more single horizontal lines of hexes.
  the column coordinate jumps by 2 because odd columns are offset downwards by half a hexagon
  This function is typically used to overwrite a normal colored hexagon with CX that makes the
  hexagon invisible
   */
  def fillHorizontalBorder(
      row: Int,
      height: Int,
      arrayWidth: Int,
      color: Int
  ): Unit =
    //  println("fillHorizontalBorder row:" + row + " height:" + height + " color:" + color)
    var thisRow = row
    val lastRow = row + height - 1
    while thisRow <= lastRow do
      var col = thisRow & 1
      var n = 0
      while n < arrayWidth do
        val hh = hexArray(n)(thisRow)
        hexArray(n)(thisRow) = HH(col, thisRow, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
        col += 2
        n += 1
      end while
      thisRow += 1
    end while
  end fillHorizontalBorder

  /*
  fillVerticalBorder is used to set the color of one vertical line of hexes.
  the column coordinate jumps is adjusted because odd columns are offset downwards by half a hexagon
  This function is typically used to overwrite a normal colored hexagon with CX that makes the
  hexagon invisible
   */

  def fillVerticalBorder(col: Int, row: Int, height: Int, color: Int): Unit =
    //  println("fillVerticalBorder col:" + col + " row:" + row + " height:" + height)
    var y = row & 1
    while y < height do
      val hh = hexArray(col)(y)
      if (y & 1) == 1 then hexArray(col)(y) = HH((col * 2) + 1, y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
      else hexArray(col)(y) = HH(col * 2, y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
      end if
      y += 2
    end while
  end fillVerticalBorder

  /*
  qrsFromXY generates the cubic coordinates (q,r,s) from the cartesian coordinates (x,y)
   */
  def qrsFromXY(x: Int, y: Int): (Int, Int, Int) =
    (x, (y - x) / 2, (-y - x) / 2)

  /*
  xyFromQRS generates the cartesian coordinates (x,y) from the cubic coordinates (q,r,s)
   */
  def xyFromQRS(q: Int, r: Int, s: Int): (Int, Int) =
    (q, (2 * r) + q)

  /*
  getQRSofTopCentreHex supplies the cubic cordinates of the top centre hex
   */
  def getQRSofTopCentreHex(width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    var y: Int = 0
    if ((width - 1) & 1) == 1 then y = 1
    else y = 0
    end if
    return (hexArray(x)(y).q, hexArray(x)(y).r, hexArray(x)(y).s)
  end getQRSofTopCentreHex

  /*
  getQRSofBottomCentreHex supplies the cubic cordinates of the bottom centre hex
   */
  def getQRSofBottomCentreHex(width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    var y: Int = 0
    if ((width - 1) & 1) == 1 then y = height - 1
    else y = height - 2
    end if
    return (hexArray(x)(y).q, hexArray(x)(y).r, hexArray(x)(y).s)
  end getQRSofBottomCentreHex

  /*
  trimBoard sets the color of the 4 sets of hexes in each corner of the rectangular array such
  that the remaining hexes that have not been touched, form a large hexagon. This function
  is used help form the inital state of the game board
   */
  def trimBoard(width: Int, height: Int, color: Int): Unit =
    val topQRS = getQRSofTopCentreHex(width, height)
    val bottomQRS = getQRSofBottomCentreHex(width, height)
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val hh = hexArray(x)(y)
        if (hh.s >= topQRS._3) || (hh.r <= topQRS._2) || (hh.s <= bottomQRS._3) || (hh.r >= bottomQRS._2)
        then hexArray(x)(y) = HH(hh.x, hh.y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
        end if

        x += 1
      end while
      y += 1
    end while
  end trimBoard

  /* 
  establishCylinderHome sets up extra black hex for the starting point for cylinder pieces
   */
  def establishCylinderHome(width: Int, height: Int, color: Int): Unit =
    val p = GetHomePos1()    
    val hh = hexArray(p.x)(p.y)
    hexArray(p.x)(p.y) = HH(hh.x, hh.y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
  end establishCylinderHome

  /* 
  establishBlockHome sets up extra black hex for the starting point for block pieces
   */
  def establishBlockHome(width: Int, height: Int, color: Int): Unit =
    val p = GetHomePos2()    
    val hh = hexArray(p.x)(p.y)
    hexArray(p.x)(p.y) = HH(hh.x, hh.y, color, hh.q, hh.r, hh.s, hh.xP, hh.yP)
  end establishBlockHome

  /*
  changeScale changes the scale of the board. At the moment (for test purposes) the scale is incremented
  in a modulo fashion on each call such that 0.2 <= scale <= 2. For testing purposes this function is invoked
  by right mouse button
   */
  def changeScale(fS: Double): Unit =
    println("changeScale to: " + fS)
    calculateXpYp(fS)
    hexBoardUpdated = true
  end changeScale

  /*
  calculateXpYp calculates the positions of the origins for the graphics used to paint each cell
  This function is invoked when the board is first established and when the scale changes
   */
  def calculateXpYp(fS: Double): Unit =
    val xWidth = boardCfg.xWidth    // amount to add to a hex centre x coord to reach the vertical line of the next column
    val yHeight = boardCfg.yHeight  // half the amount to add to a hex centre y coord to reach the next hexagon below
    val xMultiplier = xWidth * fS   // predetermine multiplier to speed things up
    val yMultiplier = yHeight * fS  // predetermine multiplier to speed things up

    var y = 0
    while y < arrayHeight do
      var x = 0
      while x < arrayWidth do
        val hh = hexArray(x)(y)
        val xP = math.round(hh.x * xMultiplier).toInt
        val yP = math.round(hh.y * yMultiplier).toInt
        hexArray(x)(y) = HH(hh.x,hh.y,hh.c,hh.q,hh.r,hh.s,xP,yP) // writing xP and yP away
        x += 1
      end while
      y += 1
    end while
  end calculateXpYp

  def getXpYp(pSrc: Point) : Point =
    var pValidated = Point(0,0)
    val x = pSrc.x
    val y = pSrc.y
    if (x >= 0) && (x < arrayWidth) && (y >= 0) && (y < arrayHeight) then
      pValidated = pSrc
    val pResult = Point(hexArray(x)(y).xP, hexArray(x)(y).yP)
    pResult

  def isThisHexBlack(hexPosn: Point) : Boolean =
    (hexArray(hexPosn.x)(hexPosn.y).c == CK)
      
  /*
  paint supplies the "SceneUpdateFragment" that contains all the graphics required to paint the hexboard
  Experience shows that this routine is time critical, so optimisation is key
   */
  def paint(fS: Double): SceneUpdateFragment =
    if hexBoardUpdated then
      // we are beginning a new calculation for the board position, scale and or size
      hexFragsCombined = SceneUpdateFragment.empty  // start this combination with an empty update
      val pB = boardCfg.pB                          // Base Corner (Top LHS) of Rectangle containing board
      val xWidth = boardCfg.xWidth                  // amount to add to a hex centre x coord to reach the vertical line of the next column
      val yHeight = boardCfg.yHeight                // half the amount to add to a hex centre y coord to reach the next hexagon below
      val xMultiplier = xWidth * fS                 // predetermine multiplier to speed things up
      val yMultiplier = yHeight * fS                // predetermine multiplier to speed things up

      var y = 0
      while y < arrayHeight do
        var x = 0
        while x < arrayWidth do
          val hh = hexArray(x)(y)
          if hh.c != CX then
            // this hex is visible so paint it
            val layer = gHex.modifyMaterial(_.withTint(mix(hh.c)))
            val frag = SceneUpdateFragment(
              Layer(layer.moveTo(pB.x + hh.xP, pB.y + hh.yP).scaleBy(fS, fS))
            )
            hexFragsCombined = hexFragsCombined |+| frag
          end if
          x += 1
        end while
        y += 1
      end while
      hexBoardUpdated = false
    end if
    hexFragsCombined // returns the previous calculation if updateNeeded is false
  end paint

  /*
  hexXYCoordsFromDisplayXY takes the mouse display coordinates (pDs) and converts them
  to a Point containing the X,Y indices into the underlying hex that matches pDs
  At the moment, if there is no underlying hex, then the resulting point defaults to (0,0)
   */

  def hexXYCoordsFromDisplayXY(pDs: Point, fS: Double): Point =
    //println("hexXYFromDisplayXY START:" + pDs)
    val GWIDTH = boardCfg.gWidth                                // The Hex graphic width without overlap of one pixel
    val GHEIGHT = boardCfg.gHeight                              // The Hex graphic height without overlap of one pixel
    val xWidth = boardCfg.xWidth                                // amount to add to a hex centre x coord to reach the vertical line of the next column
    val yHeight = boardCfg.yHeight                              // half the amount to add to a hex centre y coord to reach the next hexagon below
    val pB = boardCfg.pB                                        // Base Corner (Top LHS) of Rectangle containing board
    val sZ = boardCfg.getSideSize()                             // number of major hex rings (7 mini hexes) constituting a side
    val width = (((6 * sZ) - 2) * xWidth)                       // calculating board dimensions where xwidth is the small hexagon display width
    val height = (((6 * sZ) - 1) * yHeight * 2) - yHeight       // calculating board dimensions where yHeight is only half the small hexagon display height
    val widthScaled = math.round((width * fS)).toInt            // scaling board dimensions
    val heightScaled = math.round((height * fS)).toInt          // scaling board dimensions
    val gWidthScaled = math.round(((GWIDTH / 2) * fS)).toInt    // scaling the dimensions of the original hex
    val gHeightScaled = math.round(((GHEIGHT / 2) * fS)).toInt  // scaling the dimensions of the original hex
    val pC1 = Point(pB.x + gWidthScaled, pB.y + gHeightScaled)  // PC1 is top LH corner of the detection rectangle
    val pC2 = Point(pC1.x + widthScaled, pC1.y + heightScaled)  // pC2 is bottom RH corner of the detection rectangle
    val xHalfway = boardCfg.xHalfway                            // xcoord of halfway along the top left diagonal line of first hex
    val xH = (xHalfway * fS).toInt                              // scaling the tiny offset required for detection grid alignment

    //println("hexXYFromDisplayXY BOUNDARIES:: " + pC1 + " :: " + pC2)
    var hexXYCoords = Point(0, 0)

    // The detection grid needs to start halfway up the top LH diagonal of the first hex which (before scaling) is 10,20)
    if (pDs.x >= pC1.x + xH) && (pDs.x < pC2.x - xH) && (pDs.y >= pC1.y) && (pDs.y < pC2.y)
    then
      // we know that point pDs is valid, ie it is in the detection rectangle
      val offsetX = pDs.x - pB.x - xH
      val xWidthScaled = math.round((xWidth * fS)).toInt
      val x = math.round(offsetX / xWidthScaled).toInt
      val yHeightScaled = math.round(yHeight * fS).toInt
      val offsetY = pDs.y - pB.y - ((x & 1) * yHeightScaled)
      val y =
        ((offsetY / yHeightScaled) & 0xfffe) + (x & 1) // << this enforces  ((x & y are even) || (x & y are odd))

      //println("hexXYFromDisplayXY OFFSETS X/Y " + offsetX + ":" + offsetY + " POS X/Y " + x + ":" + y + " W:" + xWidth + " H:" + yHeight)

      if hexArray(x / 2)(y).c != CX then  // exclude hexes from display if color is CX
        hexXYCoords = Point(x / 2, y)     // x/2 because hexArray has even/odd columns
      end if
    end if
    println("hexXYFromDisplayXY FINISH:" + hexXYCoords)
    hexXYCoords
  end hexXYCoordsFromDisplayXY

  def GetHomePos1() : Point = 
    Point(0,3)
  end GetHomePos1

  def GetHomePos2() : Point = 
    if ((sZ & 1) == 0) then             
      Point(arrayWidth-2,arrayHeight-3) // even sizes
    else
      Point(arrayWidth-2,arrayHeight-5) // odd sizes
    end if
  end GetHomePos2

end HexBoard
