package game

import indigo.*
import indigoextras.ui.* 

// the original extensions to Button and Pointers was kindly provided by Dave Smith

/** This is a workaround to show a way to make buttons support simple pointer events. It is a simplified version of the
  * standard Button update function.
  */
extension (b: Button)
  def updateFromPointers(p: Pointers): Outcome[Button] =
    val inBounds = b.bounds.isPointWithin(p.position)

    val hoverEvents: Batch[GlobalEvent] =
      if inBounds && p.moved then b.onHoverOver()
      else Batch.empty
    
    val upEvents: Batch[GlobalEvent] =
      if inBounds && p.released then b.onUp()
      else Batch.empty

    val downEvents: Batch[GlobalEvent] =
      if inBounds && p.pressed then b.onDown()
      else Batch.empty

    val pointerEvents: Batch[GlobalEvent] =
      downEvents ++ upEvents ++ hoverEvents

    b.state match
      // Stay in Up state if OUT OF BOUNDS and pointer released, pressed or moved
      case ButtonState.Up if !inBounds && (p.pressed || p.released || p.moved) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      // Move from Up to Down state when pointer within bounds and pressed
      case ButtonState.Up if inBounds && p.pressed =>
        Outcome(b.toDownState).addGlobalEvents(b.onHoldDown() ++ pointerEvents)

      // Move from Up to Over state when pointer within bounds and released
      case ButtonState.Up if inBounds && p.released =>
        Outcome(b.toOverState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Move from Up to Over state when pointer moves within bounds
      case ButtonState.Up if inBounds && p.moved =>
        Outcome(b.toOverState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Move from Down to Up state if OUT OF BOUNDS and pointer released, pressed or moved
      case ButtonState.Down if !inBounds && (p.pressed || p.released || p.moved) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      // Stay in Down state if double button press
      case ButtonState.Down if inBounds && p.pressed =>
        Outcome(b).addGlobalEvents(b.onHoldDown() ++ pointerEvents)     
        
      // Move from Down to Up (or should this be "Over") state if pointer within bounds and released
      case ButtonState.Down if inBounds && p.released =>
        Outcome(b.toUpState).addGlobalEvents(pointerEvents)

      // Move from Over to Up state if OUT OF BOUNDS and pointer released, pressed or moved
      case ButtonState.Over if !inBounds && (p.pressed || p.released || p.moved) =>
        Outcome(b.toUpState).addGlobalEvents(b.onHoverOut() ++ pointerEvents)

      // Stay in Over state if pointer within bounds and released
      case ButtonState.Over if inBounds && p.released =>
        Outcome(b.toOverState).addGlobalEvents(b.onHoverOver() ++ pointerEvents)

      // Unaccounted for states.
      case _ =>
        Outcome(b).addGlobalEvents(pointerEvents)

/** This is a workaround to make up for Pointer not exposing any convenience methods.
  */
extension (p: Pointers)

  def pressed: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerDown => true
      case _                           => false
    }

  def released: Boolean =
    p.pointerEvents.exists {
      case _: PointerEvent.PointerUp => true
      case _                         => false
    }

  def moved: Boolean = 
    p.pointerEvents.exists {
      case _: PointerEvent.PointerMove => true
      case _                           => false
    }