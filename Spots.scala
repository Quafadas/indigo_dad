package game

import indigo.*

// 28/07/24 Tried to use Point instead of(Int,Int) but encoder/decoder throws compiler errors
final case class Spots( 
  indices: Set[(Int,Int)]
):
  scribe.debug("@@@ case class Spots Start")

  def calculatePossibleMoves(model: FlicFlacGameModel) : Spots = 
    scribe.debug("@@@ case Spots calculatePossibleMoves")

    val resultingSpots : Spots = Spots(Set.empty)
    FlicFlacGameModel.findPieceSelected(model) match
      case Some(piece) => 
        scribe.debug("@@@ Spots finds a selected piece")
        Spots(Set((3,5)))
        
      case None =>
        scribe.debug("@@@ Spots does not find a selected piece")
        Spots(Set.empty)
  end calculatePossibleMoves

  def paint(model: FlicFlacGameModel) : SceneUpdateFragment =
    var frag = SceneUpdateFragment.empty
    val dSF = model.scalingFactor
    val pb = model.hexBoard3.pBase
    val layer = GameAssets.gSpot(dSF)

    for (pos <- model.possibleMoveSpots.indices) {
      val pPos = model.hexBoard3.getXpYp(Point(pos._1,pos._2))
      val spotLayer = Layer(layer.moveTo(model.hexBoard3.pBase.x + pPos.x, model.hexBoard3.pBase.y + pPos.y))
      val newFrag = SceneUpdateFragment(spotLayer)
      frag = frag |+| newFrag
    }
    frag

  scribe.debug("@@@ case class Spots Finish")
end Spots


