module Common exposing (Model, viewportSize)


viewportSize : (Int, Int)
viewportSize = (800, 800)


type alias Model =
  { location : { x : Float, y: Float, z: Float }
  , rotation : Float
  , elapsed : Float
  , fireStrength : Float
  , pointerOffset : { x: Int, y: Int }
  , canvasDimensions : { width: Int, height: Int }
  }

