module Pointer exposing (handleDown, handleMove, handleUp)

import Common exposing (DragState(..))


handleUp controller = 
  { controller | upButtonDown = False
               , downButtonDown = False
               , dragState = NoDrag }

handleDown controller = controller

handleMove controller = controller 
