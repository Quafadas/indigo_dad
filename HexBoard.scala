package game

import indigo.*

/*  class HH is the class behind each individual hex cell in the grid
    including the ones you cannot see.    
*/

case class HH(
      x: Int,      // cartesian x coordinate of centre of hex
      y: Int,      // cartesian y coordinate of centre of hex
      c: Int,      // colour and visibility of hex 
      q: Int,      // cubic q Coord (-ve to +ve = left to right)
      r: Int,      // cubic r Coord (-ve to +ve = top right to bottom left)
      s: Int,      // cubic s Coord (-ve to +ve = bottom right to top left)
      xP: Int,     // pixel x offset from base point for origin of png to paint this hex
      yP: Int      // pixel y offset from base point for origin of png to paint this hex
)

class HexBoard (originPoint: Point, size: Int) {

/*********************
 *** The Hex Board ***
 *********************
The implementation of this hexagonal grid is based on the information kindly supplied at https://www.redblobgames.com/grids/hexagons/
Thankou! to Red Blob Games

Please refer to HexInfo. pdf for a full description of the construction of the hexagonal grid

Parameter size : count of "major" hexagons (where a major hexagon is 7 minor hexagons)

This grid is "flat-top orientated" and uses a primary "cartesian coordinate" system (xCoord,yCoord) of "odd-q" for display purposes.
This grid also carries a secondary "cube coordinate" system (qCoord, rCoord, sCoord) for purposes of the gaming model 

The rows of the hexArray are interleaved with the odd rows being pushed down half a hexagon,
eg  the start of the first four rows of the hexArray grid looks like this (so this is the top left hand corner with structure abbrievated)...

HH(0,0,c,0,0,0,xP,yP),  HH(2,0,c,2,-1,-1,xP,yP),  HH(4,0,c,4,-2,-2,xP,yP),  HH(6,0,c,6,-3,-3,xP,yP)

        HH(1,1,c,1,0,-1,xP,yP), HH(3,1,c,3,-1,-2,xP,yP),  HH(5,1,c,5,-2,-3,xP,yP),  HH(7,1,c,7,-3,-4,xP,yP)

HH(0,2,c,0,1,-1,xP,yP), HH(2,2,c,2,0,-2,xP,yP),   HH(4,2,c,4,-1,-3,xP,yP),  HH(6,2,c,6,-2,-4,xP,yP)

        HH(1,3,c,1,1,-2,xP,yP), HH(3,3,c,3,0,-3,xP,yP),   HH(5,3,c,5,-1,-4,xP,yP),  HH(7,3,c,7,-2,-5,xP,yP)

HH(0,4,c,0,2,-2,xP,yP), HH(2,4,c,2,1,-3,xP,yP),   HH(4,4,c,4,0,-4,xP,yP),   HH(6,4,c,6,-1,-5,xP,yP)

NB The storage for this snippet would be HexArray(4,5) ... even though the xy coords range from (0,0) to (7,4)
*/

  val assetName = AssetName("hex2")
  val assetPath = AssetPath("assets/Hex2.png")

  val GWIDTH = 90   // The Hex2 graphic is actually 91 pixels wide, but we want each graphic to overlap by one pixel
  val GHEIGHT = 80  // The Hex2 graphic is actually 81 pixels high, but we want each graphic to overlap by one pixel

  val rHex = Rectangle(0,0,GWIDTH+1, GHEIGHT+1)
  val gHex: Graphic[Material.ImageEffects] = Graphic(rHex,2,Material.ImageEffects(AssetName("hex2")))    // Hex Grid on Layer 2
  val mHex: Graphic[Material.ImageEffects] = Graphic(rHex,2,Material.ImageEffects(AssetName("hex2")))    // Magenta Hex on Layer 3

  // The board is enclosed by a rectangle with origin top LH corner as Point pB
  // pB is added to the hex coords during paint
  val pB : Point = originPoint    // Base Corner of Rectangle containing board
  val sZ : Int = ((size-2)%5)+2   // The number of hexagonal rings (ring of 6 with centre) composing one side of the board
                                  // The formula ensures 2<=size<=6
  var fS = 1.0                    // scale factor

  // The hexagons are aligned such that they have a flat bottom and top
  // The board is aligned such that it has pointy bottom and top, in terms of flat bottom hexagons
  // Note there is an interleaving edge between neighbouring hexagons, that comes from the rows above and below

  val xWidth = 70   // amount to add to a hex centre x coord to reach the vertical line of the next column
  val yHeight = 40  // half the amount to add to a hex centre y coord to reach the next hexagon below
  val xHalfway = 10 // xcoord of halfway along the top left diagonal line of first hex


  var hexBoardUpdated = true                        // set true when paint needs to recalculate the board image
  var testHexUpdated = true
  var hexFragsCombined = SceneUpdateFragment.empty  // holds the latest calculations from the paint routine
  var testFrag = SceneUpdateFragment.empty          // the latest fragment for the test hexagon

  val width = 6*(sZ-1) + 3                          // 3,9,15,21 ...
  val height = 12*(sZ-1) + 6                        // 7,17,29,41 ...
  val arrayWidth = (width+1+2)/2                    // +2 because of borders
  val arrayHeight = height+4                        // +4 because of borders

  var hexArray = createArray(arrayWidth, arrayHeight)
  //println("hexArray size:" + sZ + " w:h" + width +":" + height + " aw:ah" + + arrayWidth +":" + arrayHeight + " .rowLength:" + hexArray.length)

  fillBoard(arrayWidth, arrayHeight, mix(CK))             // start with black board
  /*
  printBoard(arrayWidth, arrayHeight)                     // useful for debugging purposes
  */
  fillHorizontalBorder(0,2,arrayWidth, CX)                // The first two rows are an invisible border
  colorBoardHexes(2, arrayWidth, arrayHeight)             // this is the pattern of the board
  fillVerticalBorder(0, 0, arrayHeight, CX)               // first column is border
  fillVerticalBorder(arrayWidth-1, 0, arrayHeight, CX)    // last two columns are borders
  fillVerticalBorder(arrayWidth-1, 1, arrayHeight, CX)
  fillHorizontalBorder(arrayHeight-2, 2 , arrayWidth, CX) // The last two rows are an invisible border
  trimBoard(arrayWidth, arrayHeight, CX)                  // trim off the four corners (uses q,r,s coords)

  //########################################################
    
  /* 
  createArray creates an array of mini hexagons according to width and height parameters
  NB width and height are hexagonal width/height dimensions, not physical dimensions
  */ 
  def createArray(width: Int, height: Int): Array[Array[HH]] = {
    Array.ofDim[HH](width, height)
  }


  /*  
  fillBoard populates each hexArray cell
  the embedded while loops populate x,y,c,q,r,s,xP,yP, but not xP, yP
  after the while loops calculateXpyP calcualtes the paint position
  for the graphic to paint the hex cell
  */
  def fillBoard(width: Int, height: Int, color: RGBA) : Unit = {
    var row = 0
    while  (row < height) {
      var col = row & 1
      var n=0
      while (n < width) {
        val q = col
        val r = (row-col)/2
        val s = (-row -col)/2
        hexArray(n)(row) = HH(col,row,CK,q,r,s,0,0)
        col += 2
        n += 1
      }
      row += 1
    }
    calculateXpYp(fS) // calculate the hex cells paint positions
  }

  /* 
  printBoard is prints the details of each hexagon on a new line
  once a complete row is printed, an additional line of "---"
  is printed to indicate the start of a new row
  This is a debug/test funtion
  */

  def printBoard(width: Int, height: Int) : Unit = {
    var y=0
    while (y<height) {
      var x=0
      while (x<width){
        println("HH: " + hexArray(x)(y))
        x += 1
      }
      println("---")
      y += 1
    }
  }

  /* 
  colorBoardHexes generates the pattern colors for each mini hex using the
  color combinations in rowTemplate
  */
  def colorBoardHexes (row: Int, arrayWidth: Int, arrayHeight: Int) : Unit = {

    val rowTemplate : Array[Vector[Int]] = Array(
      Vector(CK,CK,CK), //0
      Vector(CR,CR,CY), //1
      Vector(CO,CP,CO), //2
      Vector(CK,CK,CK), //3
      Vector(CY,CB,CY), //4
      Vector(CG,CG,CO), //5
      Vector(CK,CK,CK), //6
      Vector(CB,CB,CR), //7
      Vector(CP,CG,CP), //8
      Vector(CK,CK,CK), //9
      Vector(CR,CY,CR), //10
      Vector(CO,CO,CP), //11
      Vector(CK,CK,CK), //12
      Vector(CY,CY,CB), //13
      Vector(CG,CO,CG), //14
      Vector(CK,CK,CK), //15
      Vector(CB,CR,CB), //16
      Vector(CP,CP,CG)  //17
    ) 
    var col = 0
    var n = 0
    var thisRow = row
    while (thisRow < arrayHeight-2) {
      if ((thisRow & 1) == 0) then 
        col = 2
        n = 1
      else 
        col = 1
        n = 0

      while(n < arrayWidth-1) {
        val offset = (sZ & 1) * 10  // an odd size requires a vertical shift of the hex grid
        val hexColor = rowTemplate((thisRow-2+offset) % rowTemplate.length)(n%3)
        val hh = hexArray(n)(thisRow)
        hexArray(n)(thisRow) = HH(col,thisRow,hexColor,hh.q,hh.r,hh.s,hh.xP,hh.yP)
        col += 2
        n += 1
      }
      thisRow += 1
    }
  }

  /* 
  fillHorizontalBorder is used to set the color of one or more single horizontal lines of hexes.
  the column coordinate jumps by 2 because odd columns are offset downwards by half a hexagon
  This function is typically used to overwrite a normal colored hexagon with CX that makes the
  hexagon invisible
  */
  def fillHorizontalBorder(row: Int, height: Int, arrayWidth: Int, color: Int) : Unit = {
  //  println("fillHorizontalBorder row:" + row + " height:" + height + " color:" + color)
    var thisRow = row
    val lastRow = row + height - 1
    while (thisRow <= lastRow ) {
      var col = thisRow & 1
      var n = 0
      while (n < arrayWidth) {
        val hh =  hexArray(n)(thisRow)
        hexArray(n)(thisRow) = HH(col,thisRow,color,hh.q, hh.r, hh.s, hh.xP, hh.yP)
        col += 2
        n += 1
      }
      thisRow += 1
    } 
  }

  /* 
  fillVerticalBorder is used to set the color of one vertical line of hexes.
  the column coordinate jumps is adjusted because odd columns are offset downwards by half a hexagon
  This function is typically used to overwrite a normal colored hexagon with CX that makes the
  hexagon invisible
  */

  def fillVerticalBorder( col: Int, row : Int, height: Int, color: Int) : Unit = {
  //  println("fillVerticalBorder col:" + col + " row:" + row + " height:" + height)
    var y = row & 1
    while (y < height) {
        val hh =  hexArray(col)(y)
        if ((y & 1) == 1) then 
          hexArray(col)(y) = HH((col*2)+1,y,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)
        else
          hexArray(col)(y) = HH(col*2,y,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)
        y += 2
      }
    }

  /* 
  fillVerticalCentreLine fills the vertical centre line with a color
  This is a debug/test function
  */
  def fillVerticalCentreLine(width: Int, height: Int, color: Int) : Unit = {
  //  println("fillVerticalCentreLine width:" + width + " height:" + height + " color:" + color)  
    val centre: Int = (width-1)/2
    if (((width-1) & 1) == 1) then 
      fillVerticalBorder(centre, 1, height, color)
    else 
      fillVerticalBorder(centre, 0, height, color)

  }

  /*
  fillTopCentreHex sets the top centre hex to a color
  This is a debug/test function
  */
  def fillTopCentreHex(width: Int, height: Int, color: Int) : Unit = {
  //  println("fillTopCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width-1)/2
    if (((width-1) & 1) == 1) then
      val hh = hexArray(centre)(1) 
      hexArray(centre)(1) = HH(2*centre+1,1,color,hh.q, hh.r, hh.s,hh.xP,hh.yP) // tested for 2,4,6
    else 
      val hh = hexArray(centre)(0)
      hexArray(centre)(0) = HH(2*centre,0,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)   // tested for 3,5    
  }

  /*
  fillTopBottomHex sets the bottom centre hex to a color
  This is a debug/test function
  */
  def fillBottomCentreHex(width: Int, height: Int, color: Int) : Unit = {
  //  println("fillBottomCentreHex width:" + width + " height:" + height + " color:" + color)
    val centre: Int = (width-1)/2
    if (((width-1) & 1) == 1) then 
      val hh = hexArray(centre)(height-1)
      hexArray(centre)(height-1) = HH(2*centre+1,height-1,color,hh.q, hh.r, hh.s,hh.xP,hh.yP) // even size
    else 
      val hh = hexArray(centre)(height-2)
      hexArray(centre)(height-2) = HH(2*centre,height-2,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)   // odd size
  }

  /*
  fillCentreHex sets the central hex to a color
  This is a debug/test function
  */
  def fillCentreHex(width: Int, height: Int, color: Int) : Unit = {
  //  println("fillCentreHex width:" + width + " height:" + height + " color:" + color)
    val x: Int = (width-1)/2
    val y: Int = height/2
    if (((width-1) & 1) == 1) then 
      val hh = hexArray(x)(y)
      hexArray(x)(y) = HH(2*x+1,y,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)       // even size
    else
      val hh = hexArray(x)(y-1)
      hexArray(x)(y-1) = HH(2*x,y-1,color,hh.q, hh.r, hh.s,hh.xP,hh.yP)     // odd size

  }

  /*
  qrsFromXY generates the cubic coordinates (q,r,s) from the cartesian coordinates (x,y)
  */
  def qrsFromXY(x: Int, y: Int) : (Int, Int, Int) = {
    (x, (y-x)/2, (-y-x)/2)
  }

  /*
  xyFromQRS generates the cartesian coordinates (x,y) from the cubic coordinates (q,r,s)
  */
  def xyFromQRS(q: Int, r: Int, s: Int): (Int, Int) = {
    (q, (2*r)+q)
  }

  /*
  getQRSofTopCentreHex supplies the cubic cordinates of the top centre hex
  */
  def getQRSofTopCentreHex(width: Int, height: Int) : (Int, Int, Int) = {
    val x: Int = (width-1)/2
    var y: Int  = 0
    if (((width-1) & 1) == 1) then
      y=1
    else 
      y=0
    return (hexArray(x)(y).q, hexArray(x)(y).r, hexArray(x)(y).s)
  }
  
  /*
  getQRSofBottomCentreHex supplies the cubic cordinates of the bottom centre hex
  */
  def getQRSofBottomCentreHex(width: Int, height: Int) : (Int, Int, Int) = {
    val x: Int = (width-1)/2
    var y: Int  = 0
    if (((width-1) & 1) == 1) then
      y=height-1
    else 
      y=height-2
    return (hexArray(x)(y).q, hexArray(x)(y).r, hexArray(x)(y).s)
  }

  /*
  trimBoard sets the color of the 4 sets in each corner of the rectangular array such
  that the remaining hexes that have not been touched form a large hexagon. This function
  is used help form the inital state of the game board
  */
  def trimBoard(width: Int, height: Int, color: Int) : Unit = {
    val topQRS = getQRSofTopCentreHex(width, height)
    val bottomQRS = getQRSofBottomCentreHex(width, height)
    var y=0
    while (y<height) {
      var x=0
      while (x<width) {
        val hh = hexArray(x)(y)
        if ((hh.s >= topQRS._3) || (hh.r <= topQRS._2 ) ||(hh.s <= bottomQRS._3) || (hh.r >= bottomQRS._2))
          hexArray(x)(y) = HH(hh.x,hh.y,color,hh.q,hh.r,hh.s,hh.xP,hh.yP)

        x += 1
      }
      y += 1
    }
  }

  /*
  changeScale changes the scale of the board. At the moment (for test purposes) the scale is incremented
  in a modulo fashion on each call such that 0.2 <= scale <= 2. For testing purposes this function is invoked
  by right mouse button
  */
  def changeScale(change : Double) : Unit = 
    fS = fS + change
    if (fS >= 2)
      fS = 0.2
    println("changeScale to: " + fS)
    calculateXpYp(fS)
    hexBoardUpdated = true
    testHexUpdated = true

  /*
  calculateXpYp calculates the positions of the origins for the graphics used to paint each cell
  This function is invoked when the board is first established and when the scale changes
  */
  def calculateXpYp(scale: Double) : Unit = 
    val xMultiplier = xWidth*fS   // predetermine multiplier to speed things up
    val yMultiplier = yHeight*fS  // predetermine multiplier to speed things up
    
    var y=0
    while (y<arrayHeight) {
      var x=0
      while (x<arrayWidth) {
        val hh = hexArray(x)(y)
        val xP = math.round(hh.x*xMultiplier).toInt
        val yP = math.round(hh.y*yMultiplier).toInt                
        hexArray(x)(y) = HH(hh.x,hh.y,hh.c,hh.q,hh.r,hh.s,xP,yP)  // writing xP and yP away
        x += 1
        }
      y += 1
      }
    
  /*
  paint supplies the "SceneUpdateFragment" that contains all the graphics required to paint the hexboard
  Experience shows that this routine is time critical, so optimisation is key
  */  
  def paint() : SceneUpdateFragment =
    if ( hexBoardUpdated ) then
      // we are beginning a new calculation for the board position, scale and or size
      hexFragsCombined = SceneUpdateFragment.empty  // start this combination with an empty update
      val xMultiplier = xWidth*fS                   // predetermine multiplier to speed things up
      val yMultiplier = yHeight*fS                  // predetermine multiplier to speed things up

      var y=0
      while (y<arrayHeight) {
        var x=0
        while (x<arrayWidth) {
          val hh = hexArray(x)(y)
          if (hh.c != CX) then
            // this hex is visible so paint it
            val layer = gHex.modifyMaterial(_.withTint(mix(hh.c)))
            val frag = SceneUpdateFragment( Layer(layer.moveTo(pB.x+hh.xP,pB.y+hh.yP).scaleBy(fS,fS) ))
            hexFragsCombined = hexFragsCombined |+| frag
          x += 1
          }
        y += 1
        }
      hexBoardUpdated = false
    hexFragsCombined // returns the previous calculation if updateNeeded is false

  /*
  hexXYCoordsFromDisplayXY takes the mouse display coordinates (pDs) and converts them
  to a Point containing the X,Y indices into the underlying hex that matches pDs
  At the moment, if there is no underlying hex, then the resulting point defaults to (0,0)
  */
    
  def hexXYCoordsFromDisplayXY(pDs: Point) : Point = {
    println("hexXYFromDisplayXY START:" + pDs)
    val width = (((6*sZ)-2)*xWidth)                                 // calculating board dimensions where xwidth is the small hexagon display width
    val height = (((6*sZ)-1)*yHeight*2) - yHeight                   // calculating board dimensions where yHeight is only half the small hexagon display height
    val widthScaled = math.round((width*fS)).toInt                  // scaling board dimensions
    val heightScaled = math.round((height*fS)).toInt                // scaling board dimensions
    val gWidthScaled = math.round(((GWIDTH/2)*fS)).toInt            // scaling the dimensions of the original hex
    val gHeightScaled = math.round(((GHEIGHT/2)*fS)).toInt          // scaling the dimensions of the original hex
    val pC1 = Point(pB.x + gWidthScaled, pB.y + gHeightScaled)      // PC1 is top LH corner of the detection rectangle
    val pC2 = Point(pC1.x + widthScaled, pC1.y + heightScaled)      // pC2 is bottom RH corner of the detection rectangle
    val xH = (xHalfway*fS).toInt                                    // scaling the tiny offset required for detection grid alignment
    
    println("hexXYFromDisplayXY BOUNDARIES:: " + pC1 + " :: " + pC2)
    var hexXYCoords = Point(0,0)

    // The detection grid needs to start halfway up the top LH diagonal of the first hex which (before scaling) is 10,20)
    if ((pDs.x >= pC1.x + xH) && (pDs.x < pC2.x - xH) && (pDs.y >= pC1.y) && (pDs.y < pC2.y)) {
      // we know that point pDs is valid, ie it is in the detection rectangle
      val offsetX = pDs.x - pB.x - xH
      val xWidthScaled = math.round((xWidth*fS)).toInt
      val x = math.round(offsetX/xWidthScaled).toInt
      val yHeightScaled = math.round(yHeight*fS).toInt
      val offsetY = pDs.y - pB.y - ((x&1)*yHeightScaled)
      val y = ((offsetY/yHeightScaled) & 0xFFFE) + (x & 1)        // << this enforces  ((x & y are even) || (x & y are odd))
      
      println("hexXYFromDisplayXY OFFSETS X/Y " + offsetX+":"+offsetY + " POS X/Y "+ x +":"+ y +" W:" + xWidth + " H:" + yHeight)

      if (hexArray(x/2)(y).c != CX)   // exclude hexes from display if color is CX
        hexXYCoords = Point(x/2,y)    // x/2 because hexArray has even/odd columns
    }
    println("hexXYFromDisplayXY FINISH:" + hexXYCoords)
    hexXYCoords
  }

  // Although these last two routines and the var testPoint are included in this class, but 
  // perhaps they need to be in a seperate class or extension as they are not longer dealing
  // with the board itself.
  // These routines are the starting point for positioning the pieces

   
  var testPoint : Point = Point(0, 0)   // testPoint is a simple coordinate inside hexArray

  /*
  testPointReposition repositions the magenta hex as appropriate
  */
  def testPointReposition(newPos: Point) : Unit = {
    testPoint = newPos
    testHexUpdated = true
  }

  /*
  paintTestHex generates a "SceneUpdateFragment" containing the new position of the Megenta Hex
  */
  def paintTestHex() : SceneUpdateFragment = {
    if (testHexUpdated) then
      val layer = mHex.modifyMaterial(_.withTint(mix(CM)))
      val xP = hexArray(testPoint.x)(testPoint.y).xP
      val yP = hexArray(testPoint.x)(testPoint.y).yP
      testFrag = SceneUpdateFragment( Layer(layer.moveTo(pB.x+xP, pB.y+yP).scaleBy(fS,fS)))
      testHexUpdated = false
    testFrag
  }
}
