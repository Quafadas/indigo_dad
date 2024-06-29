package game

import indigo.*
import indigo.scenes.*

object SceneResults extends Scene[StartUpData, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel
  
  val name: SceneName = SceneName("SceneResults")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, SceneViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters = EventFilters.Permissive

  val subSystems = Set(SSResults("Results")) 

  def updateModel(
    context: SceneContext[StartUpData], 
    model: SceneModel 
    ): GlobalEvent => Outcome[SceneModel] = 
      case ButtonsTestEvent =>
        println("Results-ButtonsTestEvent")
        Outcome(model)
      case _ => Outcome(model)
  
  def updateViewModel(
    context: SceneContext[StartUpData], 
    model: SceneModel, 
    viewModel: SceneViewModel
    ): GlobalEvent => Outcome[SceneViewModel] =
      _ => Outcome(viewModel)

  // Show some text
  // When the user clicks anywhere in the screen, trigger an event to jump to the other scene.
  def present(context: SceneContext[StartUpData], 
      model: SceneModel,
      viewModel: SceneViewModel):
         Outcome[SceneUpdateFragment] = 
          Outcome {
    
    val textResults = TextBox("Results Scene", 400, 40 )
      .withColor(RGBA.Cyan)
      .withFontSize(Pixels(30))
      .moveTo(300,0)
    
      SceneUpdateFragment(Shape
        .Box(
          Rectangle(0,0,3000,2000),
          Fill.LinearGradient(Point(0),RGBA.SlateGray,Point(3000,2000), RGBA.Magenta)))
      |+| SceneUpdateFragment(textResults)
      |+| SceneUpdateFragment(viewModel.buttonSplash.draw) 
      |+| SceneUpdateFragment(viewModel.buttonParams.draw) 
      |+| SceneUpdateFragment(viewModel.buttonGame.draw) 
//      |+| SceneUpdateFragment(viewModel.buttonResults.draw)
  }



