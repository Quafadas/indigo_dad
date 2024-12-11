package game

import indigo.*

/*  class HH is the class behind each individual hex cell in the grid
    including the ones you cannot see.
 */

case class HH3(
    x: Int, // .... cartesian x coordinate of centre of hex
    y: Int, // .... cartesian y coordinate of centre of hex
    c: Int, // .... colour and visibility of hex
    q: Int, // .... cubic q Coord (-ve to +ve = left to right)
    r: Int, // .... cubic r Coord (-ve to +ve = top right to bottom left)
    s: Int, // .... cubic s Coord (-ve to +ve = bottom right to top left)
    xR: Int, // ... original copy of xS (ie with scale factor 1)
    yR: Int, // ... original copy of yS (ie with scale factor 1)
    xS: Int, // ... pixel x scaled offset from base point for origin of png to paint this hex
    yS: Int // .... pixel y scaled offset from base point for origin of png to paint this hex
)

class HexBoard4 ():

  scribe.debug("@@@ Class HexBoard4 Start")

// format: off

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
    * HH3(0,0,c,0,0,0,xP,yP),   HH3(2,0,c,2,-1,-1,xP,yP),   HH3(4,0,c,4,-2,-2,xP,yP),   HH3(6,0,c,6,-3,-3,xP,yP)
    *            HH3(1,1,c,1,0,-1,xP,yP),  HH3(3,1,c,3,-1,-2,xP,yP),   HH3(5,1,c,5,-2,-3,xP,yP),   HH3(7,1,c,7,-3,-4,xP,yP)
    * HH3(0,2,c,0,1,-1,xP,yP),  HH3(2,2,c,2,0,-2,xP,yP),    HH3(4,2,c,4,-1,-3,xP,yP),   HH3(6,2,c,6,-2,-4,xP,yP)
    *            HH3(1,3,c,1,1,-2,xP,yP),  HH3(3,3,c,3,0,-3,xP,yP),    HH3(5,3,c,5,-1,-4,xP,yP),   HH3(7,3,c,7,-2,-5,xP,yP)
    * HH3(0,4,c,0,2,-2,xP,yP),  HH3(2,4,c,2,1,-3,xP,yP),    HH3(4,4,c,4,0,-4,xP,yP),    HH3(6,4,c,6,-1,-5,xP,yP)
    *
    * NB The storage for this snippet would be HexArray(4,5) ... even though the xy coords range from (0,0) to (7,4)
    */
// format : on

/*
...... w is arrayWidth, the number of columns in "hexArray"
...... h is arrayHight, the number of rows in "hexArray"
...... aX,aY are coords used to access the cells of "hexArray"
...... x,y are cartesian coords of hex cell
...... q,r,s are cubic coords of hex cell
...... xP,yP are display coords for cell (and these are the coords that are scaled)
*/

  val arrayWidth = 9 // .............................. forcing arrayWidth=9 (calculated from sZ=3)
  val arrayHeight = 34 // ............................ forcing arrayHeight=34 (calculated from sZ=3)
  val graphicWidth = 90 // ........................... the width of the graphic crop for each hex
  val graphicHeight = 80 // .......................... the width of the graphic crop for each hex
  val pBase = Point(260,0) // ........................ coords of invisible left hand corner
  val xWidth = 70 // ................................. amount to add to a hex centre x coord to reach the vertical line of the next column
  val yHeight = 40 // ................................ half the amount to add to a hex centre y coord to reach the next hexagon below
  val xHalfway = 10 // ............................... xcoord of halfway along the top left diagonal line of first hex
  var hexArray = createArrayOfHH(arrayWidth, arrayHeight)
  var scalingFactor: Double = 1.0 // ................. scaling factor as controlled by +/- buttons
  var hexGridLayer: Layer.Content = Layer.empty // ... Layer recalculated at start and on each +/-


  // start with black board, populates q,r,s (for debugging the helper routine printBoard can follow this line)
  fillBoard(arrayWidth, arrayHeight, mix(CK))

  // The first two rows are an invisible border
  fillHorizontalBorder(0, 2, arrayWidth, CX ) 

  // this is the pattern of the board
  colorBoardHexes(2, arrayWidth, arrayHeight ) 

  // first column is border
  fillVerticalBorder(0, 0, arrayHeight, CX) 

  // last two columns are borders
  fillVerticalBorder(arrayWidth - 1, 0, arrayHeight, CX) 
  fillVerticalBorder(arrayWidth - 1, 1, arrayHeight, CX)

  // The last two rows are an invisible border
  fillHorizontalBorder(arrayHeight - 2, 2, arrayWidth, CX ) 

  // trim off the four corners (uses q,r,s coords)
  trimBoard( arrayWidth, arrayHeight, CX ) 

  // establish extra hexes for homepositions of pieces
  establishHomeHexes( arrayWidth, arrayHeight)

  // establish the paint positions for each hex
  calculateXsYs(scalingFactor)

  // establish the first GridPaintLayer scaled to 1.0
  calculateGridPaintLayer()

  // ########################################################

  /*
  createArrayOfHH creates an array of mini hexagons according to width and height parameters
  NB width and height are hexagonal width/height dimensions, not physical dimensions
   */
  def createArrayOfHH(width: Int, height: Int): Array[Array[HH3]] =
    Array.ofDim[HH3](width, height)

  /*
  fillBoard populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  Afterwards, calculateXpyP calcualtes the paint position according to scale
  for the graphic to paint the hex cell
   */
  def fillBoard(width: Int, height: Int, color: RGBA): Unit =
    var row = 0
    while row < height do // array height
      var col = row & 1
      var n = 0
      while n < width do  // array width
        val q = col
        val r = (row - col) / 2
        val s = (-row - col) / 2
        val xP = col * xWidth
        val yP = row * yHeight
        hexArray(n)(row) = HH3(col, row, CW, q, r, s, xP, yP, xP, yP)
        col += 2
        n += 1
      end while
      row += 1
    end while
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
    val sZ = 3
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
        val offset = (sZ & 1) * 10 // an odd size requires a vertical shift of the hex grid
        val hexColor = rowTemplate((thisRow - 2 + offset) % rowTemplate.length)(n % 3)
        setHexColor(Point(n,thisRow),hexColor)
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
    //  scribe.debug("fillHorizontalBorder row:" + row + " height:" + height + " color:" + color)
    var thisRow = row
    val lastRow = row + height - 1
    while thisRow <= lastRow do
      var col = thisRow & 1
      var n = 0
      while n < arrayWidth do
        setHexColor(Point(n,thisRow),color)
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
    //  scribe.debug("fillVerticalBorder col:" + col + " row:" + row + " height:" + height)
    var y = row & 1
    while y < height do
      setHexColor(Point(col,y),color)
      y += 2
    end while
  end fillVerticalBorder

  /*
  getQRSofTopCentreHex supplies the cubic cordinates of the top centre hex
   */
  def getQRSofTopCentreHex(width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    val y: Int = if ((width - 1) & 1) == 1 then 1 else 0
    return (hexArray(x)(y).q, hexArray(x)(y).r, hexArray(x)(y).s)
  end getQRSofTopCentreHex

  /*
  getQRSofBottomCentreHex supplies the cubic cordinates of the bottom centre hex
   */
  def getQRSofBottomCentreHex(width: Int, height: Int): (Int, Int, Int) =
    val x: Int = (width - 1) / 2
    val y: Int = if ((width - 1) & 1) == 1 then height - 1 else height - 2
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
        if (hh.s >= topQRS._3) || (hh.r <= topQRS._2) || (hh.s <= bottomQRS._3) || (hh.r >= bottomQRS._2) then
          setHexColor(Point(x,y),color)
        end if
        x += 1
      end while
      y += 1
    end while
  end trimBoard

  /* 
  establishHomeHexes sets up extra black hex for the starting point for cylinder pieces
   */
  def establishHomeHexes(width: Int, height: Int): Unit =
    setHexColor(getCylinderHomePos(CB), CB)
    setHexColor(getCylinderHomePos(CG), CG)
    setHexColor(getCylinderHomePos(CY), CY)
    setHexColor(getCylinderHomePos(CO), CO)
    setHexColor(getCylinderHomePos(CR), CR)
    setHexColor(getCylinderHomePos(CP), CP)
    setHexColor(getBlockHomePos(CB), CB)
    setHexColor(getBlockHomePos(CG), CG)
    setHexColor(getBlockHomePos(CY), CY)
    setHexColor(getBlockHomePos(CO), CO)
    setHexColor(getBlockHomePos(CR), CR)
    setHexColor(getBlockHomePos(CP), CP)
  end establishHomeHexes


  /* 
  sethexColor sets the color of a hex
   */

  def setHexColor(pos: Point, col : Int) : Unit = 
    val hh = hexArray(pos.x)(pos.y)
    hexArray(pos.x)(pos.y) = HH3(hh.x, hh.y, col, hh.q, hh.r, hh.s, hh.xR, hh.yR, hh.xS, hh.yS)
  end setHexColor

  /*
  calculateXsYs calculates the positions of the origins for the graphics used to paint each cell
  This function is invoked when a resize event occurs or the scale changes
   */
  def calculateXsYs(fS: Double): Unit =
    var y = 0
    while y < arrayHeight do
      var x = 0
      while x < arrayWidth do
        val hh = hexArray(x)(y)
        val xS = math.round(hh.xR * fS).toInt
        val yS = math.round(hh.yR * fS).toInt
        hexArray(x)(y) = HH3(hh.x,hh.y,hh.c,hh.q,hh.r,hh.s,hh.xR,hh.yR,xS,yS) // writing xS and yS away
        x += 1
      end while
      y += 1
    end while
    scalingFactor = fS
  end calculateXsYs

  def calculateGridPaintLayer() : Unit = 
    hexGridLayer = Layer.empty // start this combination with an empty layer
    var y = 0
    while y < arrayHeight do
      var x = 0
      while x < arrayWidth do
        val hh = hexArray(x)(y)
        if hh.c != CX then
          // this hex is visible so paint it
          val layer = GameAssets.gHex(scalingFactor).modifyMaterial(_.withTint(mix(hh.c)))
          val scaledX = hh.xS + pBase.x
          val scaledY = hh.yS + pBase.y
          hexGridLayer = hexGridLayer |+| Layer(layer.moveTo(scaledX, scaledY))
        end if
        x += 1
      end while
      y += 1
    end while
  end calculateGridPaintLayer

  def getXsYs(pSrc: Point) : Point =
    var pValidated = Point(0,0)
    val x = pSrc.x
    val y = pSrc.y
    val pResult = Point(hexArray(x)(y).xS, hexArray(x)(y).yS)
    pResult
  end getXsYs

  // detected a valid hex (ie is it part of the board) using Array Coordinates (as a point)
  def isThisHexValid(pAxAy: Point) : Boolean = 
    (hexArray(pAxAy.x)(pAxAy.y).c != CX)
  end isThisHexValid

  // detected a valid hex (ie is it part of the board) using Cubic Coordinates
  def isThisHexValid(q: Int, r: Int, s: Int) : Boolean =
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = Point(aXaY._1, aXaY._2)
    isThisHexValid(pAxAy)
  end isThisHexValid

  // detecting a black hex using Array Coordinates (as a point)
  def isThisHexBlack(pAxAy: Point) : Boolean =
    (hexArray(pAxAy.x)(pAxAy.y).c == CK)
  end isThisHexBlack

  // detecting a black hex using Cubic Coordinates (q,r,s)
  def isThisHexBlack(q: Int, r: Int, s: Int) : Boolean =
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = Point(aXaY._1, aXaY._2)
    isThisHexBlack(pAxAy)
  end isThisHexBlack

  // detect an occupied hex using Array Coordinates (as a point)
  def isThisHexFree(pAxAy: Point, vPieces: Vector[Piece]) : Boolean = 
    vPieces.find(p=>p.pCurPos == pAxAy) match
      case Some(piece) => false
      case None => true
  end isThisHexFree

// detect an occupied hex using Cubic Coordinates (q,r,s)
  def isThisHexFree(q: Int, r: Int, s: Int, vPieces: Vector[Piece]) : Boolean = 
    val aXaY = getAxAyfromQRS(q,r,s)
    val pAxAy = Point(aXaY._1, aXaY._2)
    isThisHexFree(pAxAy, vPieces)
  end isThisHexFree

// obtain color of this hex
  def getHexColor(pos: Point) : Int = 
    hexArray(pos.x)(pos.y).c
  end getHexColor

  /*
  paint supplies the "SceneUpdateFragment" that contains all the graphics required to paint the hexboard
  Experience shows that this routine is time critical, so optimisation is key
   */
 
  def paint(model: FlicFlacGameModel, dSF: Double): Layer =
    hexGridLayer
  end paint


  /*
  getAxAyFromDisplayXY takes the mouse display coordinates (pDs) and converts them
  to a Point containing the X,Y indices into the underlying hex that matches pDs
  There is either Some(Point) or None
   */

  def getAxAyFromDisplayXY(pDs: Point, fS: Double): Option[Point] =
    //scribe.debug("hexXYFromDisplayXY START:" + pDs)
    val GWIDTH = graphicWidth // ................................... The Hex graphic width without overlap of one pixel
    val GHEIGHT = graphicHeight // ................................. The Hex graphic height without overlap of one pixel
    val pB = pBase // .............................................. Base Corner (Top LHS) of Rectangle containing board
    val width = 16 * xWidth // ..................................... calculating board dimensions where xwidth is the small hexagon display width
    val height = 33 * yHeight // ................................... calculating board dimensions where yHeight is only half the small hexagon display height
    val widthScaled = math.round((width * fS)).toInt // ............ scaling board dimensions
    val heightScaled = math.round((height * fS)).toInt // .......... scaling board dimensions
    val gWidthScaled = math.round(((GWIDTH / 2) * fS)).toInt // .... scaling the dimensions of the original hex
    val gHeightScaled = math.round(((GHEIGHT / 2) * fS)).toInt // .. scaling the dimensions of the original hex
    val pC1 = Point(pB.x + gWidthScaled, pB.y + gHeightScaled) // .. PC1 is top LH corner of the detection rectangle
    val pC2 = Point(pC1.x + widthScaled, pC1.y + heightScaled) // .. pC2 is bottom RH corner of the detection rectangle
    val xH = (xHalfway * fS).toInt // .............................. scaling the tiny offset required for detection grid alignment

    //scribe.debug("hexXYFromDisplayXY BOUNDARIES:: " + pC1 + " :: " + pC2)

    // The detection grid needs to start halfway up the top LH diagonal of the first hex which (before scaling) is 10,20)
    if (pDs.x >= pC1.x + xH) && (pDs.x < pC2.x - xH) && (pDs.y >= pC1.y) && (pDs.y < pC2.y) then
      // we know that point pDs is valid, ie it is in the detection rectangle
      val offsetX = pDs.x - pB.x - xH
      val xWidthScaled = math.round((xWidth * fS)).toInt
      val x = (offsetX / xWidthScaled).toInt
      val yHeightScaled = math.round(yHeight * fS).toInt
      val offsetY = pDs.y - pB.y - ((x & 1) * yHeightScaled)
      val y = ((offsetY / yHeightScaled) & 0xfffe) + (x & 1) // << this enforces  ((x & y are even) || (x & y are odd))

      //scribe.debug("hexXYFromDisplayXY OFFSETS X/Y " + offsetX + ":" + offsetY + " POS X/Y " + x + ":" + y + " W:" + xWidth + " H:" + yHeight)

      val c = hexArray(x / 2)(y).c
      if (c != CX) then // ...................... exclude hexes from display if color is CX
        val pAxAy = Point(x / 2, y) // .......... x/2 because hexArray has even/odd columns
        // scribe.debug("hexXYFromDisplayXY FINISH:" + hexXYCoords)
        Some(pAxAy)
      else 
        // scribe.debug("hexXYFromDisplayXY FINISHES with NONE (non-displayable hex)")    
        None
      end if
    else
      // scribe.debug("hexXYFromDisplayXY FINISHES with NONE (outside detection grid)")    
      None
    end if
  end getAxAyFromDisplayXY


  def getCylinderHomePos(id: Int): Point =
    val p1 = Point(0,1)
    val p2 = Point(arrayWidth-2,1)
    val p3 = Point(0, arrayHeight-3)
    val p4 = Point(arrayWidth-2, arrayHeight-3)
    id match
        case CB => p1 + Point(1,3)  // Blue
        case CR => p1 + Point(1,2)  // Red
        case CY => p1 + Point(2,1)  // Yellow
        case CO => p4 + Point(-1,-1)  // Orange
        case CG => p4 + Point(-1,-2) // Green
        case CP => p4 + Point(0,-3) // Purple
    end match
  end getCylinderHomePos

  def getBlockHomePos(id: Int): Point =
    val p1 = Point(0,1)
    val p2 = Point(arrayWidth-2,1)
    val p3 = Point(0, arrayHeight-3)
    val p4 = Point(arrayWidth-2, arrayHeight-3)
    id match
        case CB => p3 + Point(1,-3)  // Blue
        case CR => p3 + Point(1,-2) // Red
        case CY => p3 + Point(2,-1) // Yellow
        case CO => p2 + Point(-1,1)  // Orange
        case CG => p2 + Point(-1,2)  // Green
        case CP => p2 + Point(0,3)  // Purple
    end match
  end getBlockHomePos

  // convert from embedded qrs to embedded xy
  def getXYfromQRS(q: Int, r: Int, s:Int) : (Int, Int) =
    val x = q
    val y = q + (2*r)
    (x,y)
  end getXYfromQRS

  // convert from embedded xy to embedded qrs
  def getQRSfromXY(x: Int, y: Int) : (Int, Int, Int) = 
    val q = x
    val r = (y-x)/2
    val s = -q -r
    (q,r,s)
  end getQRSfromXY

  // convert from array xy to embedded xy
  def getXYfromAxAy(aX: Int, aY: Int) : (Int, Int) = 
    val x = (2*aX) + (aY&1)
    val y = aY
    (x,y)
  end getXYfromAxAy

  // convert from embedded xy to array xy
  def getAxAyfromXY(x: Int, y: Int) : (Int, Int) = 
    val aX = (x - (x&1))/2
    val aY = y
    (aX,aY)
  end getAxAyfromXY

  // convert from array xy to embedded qrs
  def getQRSfromAxAy(aX: Int, aY: Int) : (Int, Int, Int) = 
    val q = (2 * aX) + (aY & 1)
    val r = ((aY - (aY&1))/2) - aX
    val s = -q -r
    (q,r,s)
  end getQRSfromAxAy

  // convert from embedded qrs to array xy
  def getAxAyfromQRS(q: Int, r: Int, s:Int) : (Int, Int) =
    val aX = (q -  (q&1))/2
    val aY = q + (2*r)
    (aX,aY)
  end getAxAyfromQRS

  scribe.debug("@@@ Class HexBoard4 Finish")

end HexBoard4

